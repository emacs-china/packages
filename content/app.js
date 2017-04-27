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

    $scope.packages = {};
    $scope.pageSize = 100;

    $http.get('all.json').success(function (content) {
      $cache.set(content);
      $scope.packages = $cache.get();
    });

    $scope.count = function(packages) {
      return Object.keys(packages ? packages : $scope.packages).length;
    };

    var unique = function(arr) {
      return Object.keys(arr.reduce(function (o, v, i) {
        o[v] = i;
        return o;
      }, {}));
    };

    $scope.cache = function($event, keywords) {
      if ($event.keyCode === 32) {
        var cacheKey = unique(keywords.split(' ')).sort().join('.');

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
        var cachedKeywords = unique(keywords.split(' ')).filter(function (word) {
          return this.wordArr.indexOf(word) >= 0;
        }, {wordArr: unique(lastKeywords.split(' '))}).sort().join('.');

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
      $scope.gotoPage($scope.pageCount ? 1 : 0);
    });

    $scope.pageIndexArr = [];
    $scope.gotoPage = function(pageIndex) {
      if ($scope.pageCount < 1) {
        $scope.pageIndexArr = [];
        $scope.pageIndex = 0;
      } else if ($scope.pageCount === 1) {
        $scope.pageIndexArr = [1];
        $scope.pageIndex = 1;
      } else {
        var pageIndexArr = [];
        var count = $scope.pageCount < 9 ? $scope.pageCount : 9;
        var start = pageIndex - ((count-1) / 2);
        var end = pageIndex + ((count-1) / 2);

        if (pageIndex > $scope.pageCount) {
          pageIndex = $scope.pageCount;
        }

        if (start < 1) {
          start = 1;
          end = count;
        }

        if (end > $scope.pageCount) {
          end = $scope.pageCount;
          start = $scope.pageCount > count ? ($scope.pageCount - count + 1) : 1;
        }

        for (var i = start; i <= end; i++) {
          pageIndexArr.push(i);
        }

        $scope.pageIndexArr = pageIndexArr;
        $scope.pageIndex = pageIndex;
      }
    };

  }]);