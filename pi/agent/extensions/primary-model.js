import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';

// Keep the shared selection stable while physical models and credentials stay local.
export default function (pi) {
  pi.registerVirtualModel({
    provider: 'primary',
    id: 'default',
    name: 'Default (workstation)',
    thinkingLevels: ['off', 'minimal', 'low', 'medium', 'high', 'xhigh', 'max'],
    route(request, ctx) {
      const sticky = request.reason === 'retry' ? request.failed :
        request.reason === 'continuation' ? request.previous : undefined;
      if (sticky) return { model: sticky.model, thinkingLevel: sticky.thinkingLevel ?? request.thinkingLevel };

      // Match Pi's agent-directory override, including ~/ expansion.
      const configuredDir = process.env.PI_CODING_AGENT_DIR;
      const agentDir = configuredDir
        ? configuredDir.replace(/^~(?=$|\/)/, homedir())
        : join(homedir(), '.pi', 'agent');
      const path = join(agentDir, 'primary-model.json');
      let config;
      try {
        config = JSON.parse(readFileSync(path, 'utf8'));
      } catch (error) {
        throw new Error(`Cannot read ${path}. Create it with {"provider":"your-provider","model":"your-model-id"}. ${error.message}`);
      }
      if (!config || Array.isArray(config) ||
          typeof config.provider !== 'string' || !config.provider.trim() ||
          typeof config.model !== 'string' || !config.model.trim()) {
        throw new Error(`${path} must contain non-empty string fields "provider" and "model".`);
      }
      if (config.provider === 'primary' && config.model === 'default') {
        throw new Error(`${path}: primary/default cannot route to itself. Choose a physical model.`);
      }
      const model = ctx.modelRegistry.find(config.provider, config.model);
      if (!model) {
        throw new Error(`${path}: model ${config.provider}/${config.model} was not found. Configure the physical model in models.json or choose an installed provider's model.`);
      }
      // Pi rejects virtual targets/missing credentials and clamps thinking levels.
      return { model, thinkingLevel: request.thinkingLevel };
    },
  });
}
