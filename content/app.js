angular.module("ngApp", [])
  .filter('limit', function () {
    return function (all, start, offset) {
      var packages = {};
      var i = 0, n = 0;

      for (var name in all) {
        if (i >= start && n < offset) {
          packages[name] = all[name];
          n++;
        }
        i++;
      }

      return packages;
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
        var wordArr = keywords.split(' ');
        var matchedFn = function (str) {
          return wordArr.filter(function (word) {
            return str.indexOf(word) >= 0;
          }).length;
        };

        for (var name in all) {
          if (matchedFn(name) === wordArr.length || matchedFn(all[name].desc) === wordArr.length) {
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