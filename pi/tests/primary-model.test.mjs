import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { test } from 'node:test';

// Mutations caught: wrong target, dropped thinking level, ignored sticky model,
// swallowed config errors, and accepting a recursive target.
const extension = await import('../agent/extensions/primary-model.js').catch(() => null);

test('primary/default routes using workstation configuration', async (t) => {
  assert.ok(extension, 'primary-model extension must exist');
  const dir = mkdtempSync(join(tmpdir(), 'pi-primary-'));
  const previousDir = process.env.PI_CODING_AGENT_DIR;
  process.env.PI_CODING_AGENT_DIR = dir;
  t.after(() => {
    if (previousDir === undefined) delete process.env.PI_CODING_AGENT_DIR;
    else process.env.PI_CODING_AGENT_DIR = previousDir;
    rmSync(dir, { recursive: true, force: true });
  });
  let virtual;
  extension.default({ registerVirtualModel: (definition) => { virtual = definition; } });
  const target = { provider: 'workstation', id: 'actual-api-model' };
  const ctx = { modelRegistry: { find: (provider, id) =>
    provider === 'workstation' && id === 'actual-api-model' ? target : undefined } };
  const route = (request = {}) => virtual.route({ reason: 'user', thinkingLevel: 'high', ...request }, ctx);
  const configure = (value) => writeFileSync(join(dir, 'primary-model.json'), JSON.stringify(value));

  await t.test('missing file gives a setup error', () => {
    assert.throws(() => route(), /primary-model.json/);
  });
  await t.test('malformed JSON gives a config error', () => {
    writeFileSync(join(dir, 'primary-model.json'), '{');
    assert.throws(() => route(), /primary-model.json/);
  });
  await t.test('invalid shapes and empty identifiers are rejected', () => {
    for (const value of [null, [], {}, { provider: 'x' }, { provider: 1, model: 'x' }, { provider: ' ', model: 'x' }, { provider: 'x', model: '' }]) {
      configure(value);
      assert.throws(() => route(), /provider.*model/);
    }
  });
  await t.test('unknown models fail explicitly', () => {
    configure({ provider: 'missing', model: 'not-in-catalog' });
    assert.throws(() => route(), /missing\/not-in-catalog/);
  });
  await t.test('self-routing is rejected', () => {
    configure({ provider: 'primary', model: 'default' });
    assert.throws(() => route(), /itself/);
  });
  await t.test('routes both user and direct requests and preserves thinking level', () => {
    configure({ provider: 'workstation', model: 'actual-api-model' });
    for (const reason of ['user', 'direct']) {
      const result = route({ reason });
      assert.equal(result.model, target);
      assert.equal(result.thinkingLevel, 'high');
    }
    assert.equal(virtual.provider, 'primary');
    assert.equal(virtual.id, 'default');
    assert.deepEqual(virtual.thinkingLevels, ['off', 'minimal', 'low', 'medium', 'high', 'xhigh', 'max']);
  });
  await t.test('continuations and retries remain on the dispatched model', () => {
    const sticky = { model: { provider: 'old', id: 'previous' }, thinkingLevel: 'low' };
    assert.deepEqual(route({ reason: 'continuation', previous: sticky }), sticky);
    assert.deepEqual(route({ reason: 'retry', failed: sticky }), sticky);
    assert.equal(route({ reason: 'user', previous: sticky }).model, target);
  });
  await t.test('mapping changes are read on the next request', () => {
    configure({ provider: 'missing', model: 'new-target' });
    assert.throws(() => route(), /missing\/new-target/);
  });
});
