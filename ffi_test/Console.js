export const log = function (s) {
  return function () {
    console.log(s);
  };
};
