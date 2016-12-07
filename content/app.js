angular.module("ngApp", [])
  .filter('limit', function () {
    return function (packages, start, offset) {
      var matched = {};
      var i = 0, n = 0;

      for (var name in packages) {
        if (i >= start && n < offset) {
          matched[name] = packages[name];
          n++;
        }
        i++;
      }

      return matched;
    };
  })
  .controller('MainCtrl', ['$scope', '$http', function ($scope, $http) {
    var all = {};
    $scope.packages = all;
    $scope.pageSize = 100;

    $http.get('all.json').success(function (content) {
      all = content;
      $scope.packages = all;
    });

    $scope.count = function(packages) {
      return Object.keys(packages ? packages : $scope.packages).length;
    };

    $scope.filter = function(keywords) {
      if (!keywords) {
        $scope.packages = all;
      } else {
        var packages = {};

        // return not matched
        var remain = function (str, wordArr) {
          return wordArr.filter(function (word) {
            return str.indexOf(word) < 0;
          });
        };

        for (var name in all) {
          if (!remain(all[name].desc.toLowerCase(), remain(name.toLowerCase(), keywords.toLowerCase().split(' '))).length) {
            packages[name] = all[name];
          }
        }

        $scope.packages = packages;
      }
    };

    $scope.$watch('packages', function (newValue, oldValue) {
      $scope.pageCount = Math.ceil($scope.count(newValue) / $scope.pageSize);
      $scope.pageIndex = 0;
    });
  }]);