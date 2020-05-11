(function () {
    var wtp = {};

    wtp.awaitElement = function (sel, done) {
        var timer = setInterval(function () {
            if (document.querySelector(sel)) { clearInterval(timer); done(); }
        }, 100);
    }


    wtp.isElementVisible = function (el) {
        const cs = window.getComputedStyle(el);
        return (
            cs.getPropertyValue('display') !== 'none'
                && cs.getPropertyValue('visibility') !== "hidden"
                && cs.getPropertyValue('opacity') !== "0"
                && el.offsetParent !== null
        );
    }

    window.wtp = wtp;

})();
