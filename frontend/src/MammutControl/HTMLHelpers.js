exports.initFormSelects = function() {
  const elems = document.querySelectorAll("select");
  return M.FormSelect.init(elems, {});
};
