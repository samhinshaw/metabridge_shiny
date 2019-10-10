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
// $(document).ready(() => {
//   // Disable viz panel on page load
//   $("a[data-value='vizPanel']")
//     .parent()
//     .addClass('disabled');
//   // Add btn-tooltip class
//   $("a[data-value='vizPanel']").addClass('panel-tooltip');
//   // Add title class
//   $("a[data-value='vizPanel']").attr(
//     'title',
//     'Please select a metabolite that has been mapped via KEGG'
//   );
//   // Initialize JQuery tooltips (but we're not using them anymore!)
//   // $('[data-toggle="tooltip"]').tooltip();
// });

const handlers = {
  // we're passing a static object, so this will only run ONCE.
  // However, we could link this to button presses to make the loading even more dynamic
  lazyLoadPackages: num => {
    // First argument is input name
    // Second argument is value to send
    Shiny.onInputChange('sessionInitialized', num);
  },
  initGetStarted: () => {
    const getStartedButton = document.getElementById('getStarted');
    const tutorialButton = document.getElementById('tutorial');
    const aboutButton = document.getElementById('about');
    // change innerHTML
    getStartedButton.innerHTML = 'Get Started';
    // remove disabled class
    getStartedButton.classList.remove('disabled');
    // Unhide tutorial button
    tutorialButton.classList.remove('btn-hidden');
    // Unhide about button
    aboutButton.classList.remove('btn-hidden');
    // add btn-tooltip class
    getStartedButton.classList.add('btn-tooltip');
    // Add title
    getStartedButton.setAttribute('title', "Let's Go!");
    // activate tippy on this button!
    handlers.activateTooltips(['#getStarted']);
  },
  addToolTips: () => {
    // Disable viz panel on page load
    $("a[data-value='vizPanel']")
      .parent()
      .addClass('disabled');
    // Add tooltip class
    $("a[data-value='vizPanel']").addClass('panel-tooltip');
    // Add title class
    $("a[data-value='vizPanel']").attr(
      'title',
      'Please select a metabolite that has been mapped via KEGG'
    );
  },
  activateTooltips: selectors => {
    const tippyOptions = {
      size: 'big',
      duration: 150
    };
    selectors.map(selector => {
      tippy(selector, tippyOptions);
    });
  }
};

// We must use shiny:sessioninitialized, not DOM Content Loaded
$(document).on('shiny:sessioninitialized', () => {
  handlers.lazyLoadPackages(1);
  handlers.addToolTips();
  handlers.activateTooltips(['.panel-tooltip', '.btn-tooltip']);
  // handlers.activateTooltips('.btn-tooltip');
});

window.onbeforeunload = () => {
  // First check to see whether Shiny has disconnected
  if (document.getElementById('shiny-disconnected-overlay') == null) {
    // If Shiny is NOT disconnected, confirm exit
    return 'If you navigate away, you will lose all of your intermediate results! Are you sure?';
  }
};

// document.addEventListener('DOMContentLoaded', () => {
//   handlers.lazyLoadPackages(1);
// });
