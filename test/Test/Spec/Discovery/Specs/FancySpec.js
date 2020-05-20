"use strict";

exports.jsQuicksort = function (array) {
  return function () {
    quicksort(array, 0, array.length - 1);
    return array;
  };
};

// from https://github.com/purescript-contrib/purescript-unsafe-reference/blob/master/src/Unsafe/Reference.js
exports.reallyUnsafeRefEq = function (a) {
  return function (b) {
    return a === b;
  };
};

// from https://www.guru99.com/quicksort-in-javascript.html
function swap(items, leftIndex, rightIndex) {
  var temp = items[leftIndex];
  items[leftIndex] = items[rightIndex];
  items[rightIndex] = temp;
}
function partition(items, left, right) {
  var pivot = items[Math.floor((right + left) / 2)], //middle element
    i = left, //left pointer
    j = right; //right pointer
  while (i <= j) {
    while (items[i] < pivot) {
      i++;
    }
    while (items[j] > pivot) {
      j--;
    }
    if (i <= j) {
      swap(items, i, j); //swapping two elements
      i++;
      j--;
    }
  }
  return i;
}

function quicksort(items, left, right) {
  var index;
  if (items.length > 1) {
    index = partition(items, left, right); //index returned from partition
    if (left < index - 1) {
      //more elements on the left side of the pivot
      quicksort(items, left, index - 1);
    }
    if (index < right) {
      //more elements on the right side of the pivot
      quicksort(items, index, right);
    }
  }
  return items;
}
