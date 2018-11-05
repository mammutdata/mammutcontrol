exports.initFormSelects = function() {
  const elems = document.querySelectorAll("select");
  return M.FormSelect.init(elems, {});
};

exports.initModals = function() {
  const elems = document.querySelectorAll(".modal");
  return M.Modal.init(elems, {});
};
