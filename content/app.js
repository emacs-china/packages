angular.module("ngApp", [])
  .controller('MainCtrl', ['$scope', '$http', function ($scope, $http) {
    var all = {};
    $scope.packages = all;

    $http.get('all.json').success(function (content) {
      all = content;
      $scope.packages = all;
    });

    $scope.count = function() {
      return Object.keys(all).length;
    };

    $scope.filter = function(keyword) {
      if (!keyword) {
        $scope.packages = all;
      } else {
        var packages = {};

        for (var name in all) {
          if (name.indexOf(keyword) >= 0 || all[name].desc.indexOf(keyword) >= 0) {
            packages[name] = all[name];
          }
        }

        $scope.packages = packages;
      }
    };
  }]);