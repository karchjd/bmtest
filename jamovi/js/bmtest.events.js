
const events = {
    update: function(ui) {
        updateUI(ui, this);
    },

    onChange_asym: function(ui) {
        updateUI(ui, this);
    },

    onChange_randomPerm: function(ui) {
        updateUI(ui, this);
    }

};

const updateUI = function(ui, context) {

    let asymNames = ui.asym.value();
    let randomPermNames = ui.randomPerm.value();

    if ( ! asymNames && ! randomPermNames ) {
        ui.ci.setValue(false)
    }
    else {
        // ui.relEff.setPropertyValue('enable', true);
    }

};

module.exports = events;
