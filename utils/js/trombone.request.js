var Trombone = Trombone || {};

Trombone.request = (function($){

    function serial(data) {
        return 'string' === typeof(data) ? data : JSON.stringify(data);
    }

    function request(conf) {

        var source = conf.resource.replace(/^\/|\/$/g, ''),
            host = conf.host.replace(/\/$/, ''),
            client = conf.client || 'generic';

        var data = conf.data ? serial(conf.data) : '';

        var pieces = client 
             + ':' + (conf.type || 'GET')
             + ':' + '/' + source
             + ':' + conf.nonce
             + ':' + data;

        var hash = CryptoJS.HmacSHA1(pieces, conf.key),
            onComplete = conf.complete;

        delete(conf.data);
        delete(conf.complete);
        $.support.cors = true;

        $.ajax($.extend({
            type         : 'GET',
            url          : host + '/' + source,
            cache        : false,
            data         : data,
            crossDomain  : true,
            dataType     : 'json',
            complete     : function() {
                if (onComplete && 'function' === typeof onComplete) { 
                    onComplete(); 
                } 
            },
            headers: {
                "Accept"       : "application/json; charset=utf-8",
                "Content-Type" : "application/json; charset=utf-8",
                "API-Access"   : client + ':' + conf.nonce + ':' + hash.toString()
             }
        }, conf));

        if (conf.debug)
            console.log('@' + host + '/' + source);
    }

    return request;

}(jQuery));
