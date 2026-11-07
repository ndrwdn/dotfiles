/** @type {import("./npm/node_modules/pi-vimmode/config").VimConfig} */
export default (vim) => {
  // Normal-mode Return moves down to the first nonblank character.
  // Insert-mode Return and Shift+Return remain owned by Pi.
  vim.keymap.set("n", "<enter>", "j^", {
    allowProtected: true,
    desc: "Move to next line's first nonblank character",
  });
};
