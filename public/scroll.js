var scrollMiddle = function (str) {
    // FIND VALUES
    // searchVal : Input
    let searchVal = document.getElementById('search_input').value;
    if(!searchVal) return false; // if search if empty return and so we don't do anything
    // diceElms : Array [ Div ]
    let diceElms = Array.from(document.querySelectorAll('.dice_item'));
    // searchVals : searchVal -> Array [ String ]
    let searchVals = searchVal.split('');
    // FIND RANK
    // diceElms : Array [ Obj { rank : Int, el : Div } ]
    diceElms = diceElms.map(function (diceEl) {
        // id : String
        let id = diceEl.getAttribute('id');
        // parts : Array [ String ]
        let parts = id.split('');
        // rank : _ -> Int
        let rank = (function () {
            let r = 0;
            if (searchVals.length < 1) return;
            if (parts[0] == searchVals[0]) r++;
            if (searchVals.length > 1 && r == 1 && parts[1] == searchVals[1]) r++;
            if (searchVals.length > 2 && r == 2 && parts[2] == searchVals[2]) r++;
            if (searchVals.length > 3 && r == 3 && parts[3] == searchVals[3]) r++;
            if (searchVals.length > 4 && r == 4 && parts[4] == searchVals[4]) r++;
            return r;
        })();
        return { rank: rank, el: diceEl };
    });
    // diceElm : Div
    diceElm = diceElms.reduce(function (c, a) {
        if (a.rank > c.rank) return a;
        else return c;
    }, {rank: -1});
    // box : Div
    let box = document.querySelector('.dice_master');
    box.scrollTop = diceElm.el.offsetTop - (box.clientHeight / 2);
};
