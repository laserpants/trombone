var Trombone = Trombone || {};

Trombone.request = (function($){

    function serial(data) {
        return 'string' === typeof(data) ? data : JSON.stringify(data);
    }

    function request(conf) {

        var source = conf.resource.replace(/^\/|\/$/g, ''),
            host = conf.host.replace(/\/$/, ''),
            data = conf.data ? serial(conf.data) : '',
            hash = CryptoJS.HmacSHA1(data, conf.key),
            onComplete = conf.complete,
            client = conf.client || 'generic';

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
                "API-Access"   : client + ':' + hash.toString()
             }
        }, conf));

        if (conf.debug)
            console.log('@' + host + '/' + source);
    }

    return request;

}(jQuery));

