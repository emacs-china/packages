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
    var $cache = (function() {
      var pool = {};
      return {
        set: function(packages, key) {
          pool[key ? key : ''] = packages;
        },
        get: function(key) {
          return pool[key ? key : ''];
        },
        prune: function() {
          pool = {'': pool['']};
        }
      };
    })();

    $scope.packages = $cache;
    $scope.pageSize = 100;

    $http.get('all.json').success(function (content) {
      $cache.set(content);
      $scope.packages = $cache.get();
    });

    $scope.count = function(packages) {
      return Object.keys(packages ? packages : $scope.packages).length;
    };

    $scope.cache = function($event, keywords) {
      if ($event.keyCode === 32) {
        var cacheKey = keywords.split(' ').sort().join('.');
        if (!$cache.get(cacheKey)) {
          $cache.set($scope.packages, cacheKey);
        }
      }
    };

    var lastKeywords = '';
    $scope.filter = function(keywords) {
      if (!keywords) {
        $scope.packages = $cache.get();
        $cache.prune();
      } else {
        var packages = {};

        // return not matched
        var remain = function (str, wordArr) {
          return wordArr.filter(function (word) {
            return str.indexOf(word) < 0;
          });
        };

        // find intersection
        var cachedKeywords = keywords.split(' ').filter(function (word) {
          return this.wordArr.indexOf(word) >= 0;
        }, {wordArr: lastKeywords.split(' ')}).sort().join('.');

        var cachedPackages = $cache.get(cachedKeywords);
        if (!cachedPackages) {
          cachedPackages = $cache.get();
        }

        for (var name in cachedPackages) {
          if (!remain(cachedPackages[name].desc.toLowerCase(), remain(name.toLowerCase(), keywords.toLowerCase().split(' '))).length) {
            packages[name] = cachedPackages[name];
          }
        }

        $scope.packages = packages;
        lastKeywords = keywords;
      }
    };

    $scope.$watch('packages', function (newValue, oldValue) {
      $scope.pageCount = Math.ceil($scope.count(newValue) / $scope.pageSize);
      $scope.pageIndex = 0;
    });
  }]);