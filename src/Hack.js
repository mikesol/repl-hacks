exports.wag_ = function (d) {
  return function () {
    var wi = window.__w4g$_it;
    if (wi && wi.value0 === "__w4g__") {
      return wi;
    }
    return d;
  };
};
