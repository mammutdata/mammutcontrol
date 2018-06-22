exports.bindSideNav = function() {
  const elem = document.getElementById("slide-out");
  return M.Sidenav.init(elem, {});
};

exports.unbindSideNav = function(inst) {
  return function() {
    if(inst) { inst.destroy(); }
  }
};
