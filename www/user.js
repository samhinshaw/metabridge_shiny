"use strict";
//
// const data = {
//   columns: []
//   addColumns: function(column) {
//     this.columns.push({
//
//     })
//   }
// };
//
// const handlers = {
//   addColums: function( {
//     let columsPicked = document.getElementById("columnsPicked");
//     columns.
//   })
// };

// const lisToChange = document.getElementById("addTodoTextInput");
// const matchingElemtents = [];
// const helpNavBar = document.querySelectorAll('li > a[data-value="Help"]');
// const aboutNavBar = document.querySelectorAll('li > a[data-value="About"]');

// JQuery code to *effectively* move navbar elements to right! This works, and is probably the best way to do it in the future... HOWEVER, it causes problems with shiny knowing what tab you're on, as it's technically a separate list from the #navbarLayout list. Also, it has a pop-in, which is visually unappealing. This cannot be easily styled with CSS without messing with the responsiveness of the site overall.

// $(document).ready(function() {
//   $("#navbarLayout").after(
//     '<ul class="nav navbar-nav navbar-right shiny-tab-input shiny-bound-input" id="navbarLayoutRight"></ul>'
//   );
//   var aboutElement = $('li:has(> a[data-value^="About"])').detach();
//   var helpElement = $('li:has(> a[data-value^="Help"])').detach();
//   $("#navbarLayoutRight").append(aboutElement);
//   $("#navbarLayoutRight").append(helpElement);
// });
//
// Initialize Tooltips with JQuery
$(document).ready(function() {
  $('[data-toggle="tooltip"]').tooltip();
});
