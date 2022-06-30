// create a binding object
var likertRadioButtonsBinding = new Shiny.InputBinding();

// add methods to it using jQuery's extend method
$.extend(likertRadioButtonsBinding, {

  find: function(scope) {
    // find all instances of class
    return $(scope).find(".shiny-input-likert-radiobuttons");
  },
  initialize: function(el) {
    // bind function onchange to update indicator

    var indicators = document.getElementById(Shiny.$escape(el.id)).
    getElementsByClassName('likert-input-radio indicator-updater')
    // only if the indicator is there!

    if (indicators.length != 0) {
      $(indicators).change(function(){
         var text=$(this).attr('choice-name');
         var id=$(this).attr('name');
         document.getElementById(id).getElementsByClassName('likert-input-radio-indicator')[0].textContent=text;
       });
    };
  },
  getValue(el) {
    // Select the radio objects that have name equal to the grouping div's id
    const checkedItems = $(
      'input:radio[name="option1"]:checked'
    );

    if (checkedItems.length === 0) {
      return null;
    }

    return checkedItems.val();
  },
  setValue(el, value) {
    if ($.isArray(value) && value.length === 0) {
      // Removing all checked item if the sent data is empty
      $('input:radio[name="option1"]').prop("checked", false);
    } else {
      $(
        'input:radio[name="option1"][value="' +
          value +
          '"]'
      ).prop("checked", true);
    }
    $(el).trigger("change");
  },
  subscribe(el, callback) {
    $(el).on("change.likertRadioButtonsBinding", function () {
      callback(false);
    });
  },
  unsubscribe(el) {
    $(el).off(".likertRadioButtonsBinding");
  }
});
Shiny.inputBindings.register(likertRadioButtonsBinding);