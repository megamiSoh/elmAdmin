import './css/bulma.css';
import './main.css';
import './css/all.min.css'
import './css/app.css'
import './css/datepicker.css'

import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

if(!!document.createRange) {
  document.getSelection().removeAllRanges();
}
var agent = navigator.userAgent.toLowerCase();
var filtercheck;
if (agent.indexOf("msie") != -1) {
  alert("인터넷익스플로러 브라우저입니다.");
  
  }

  window.addEventListener('resize', () => {
    const windowWidth = window.outerWidth;
    windowWidth > 450 ? filtercheck = false : filtercheck = true
});

const url ='http://13.209.49.169:4000/api/v1/'
// const url = 'https://api.yfit.co.kr/api/v1/'
var filter = "win16|win32|win64|mac|macintel"; 

if (filter.indexOf( navigator.platform.toLowerCase() ) > 0) {
  filtercheck = filter.indexOf( navigator.platform.toLowerCase() ) < 0
}

var flags = 
  { token : 
    localStorage.getItem("token"), 
  checkBrowser : filter.indexOf( navigator.platform.toLowerCase() ) < 0
  }
  var something = (function() {
    var executed = false;
    return function() {
        if (!executed) {
            executed = true;
            alert ("로그아웃되었습니다.")
            location.replace("/")
        }
    };
})();



var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags : flags
});


app.ports.sendData.subscribe(function () {
  var val = localStorage.getItem("addItem")
  if(val != undefined) {
    app.ports.receiveData.send(JSON.parse(val))
    
  }
 
});

app.ports.dateValidate.subscribe(function (date){
  let dateFormat = date.split(',')
  let dateCheck = new Date (date.replace(/,/g,'/'))
  let oldDate = new Date(1900/1/1) < dateCheck
  let validate =  dateFormat[0] == dateCheck.getFullYear() && (dateFormat[1] - 1)  == dateCheck.getMonth() && dateFormat[2] == dateCheck.getDate() && new Date () > dateCheck && oldDate
  app.ports.dateValidResult.send(validate)
})

app.ports.hideFooter.subscribe(function () {
  if (document.getElementById("myElement") == null) { return; 
  } else {
    document.getElementById("myElement").innerHTML = "";
    document.getElementById("myElement").classList.remove(document.getElementById("myElement").classList[0], document.getElementById("myElement").classList[1])
  }
  if (document.getElementById("footer") == null) {
    return;
  } else {
    var footer = document.getElementById("footer")
    footer.style.display == "" ? footer.style.display = "none" : footer.style.display = ""
  }
})

app.ports.deleteData.subscribe(function() {
  localStorage.removeItem ("addItem")
})

app.ports.scrollRight.subscribe(function() {
  var target = document.getElementById("scrollCtr")
  target.scrollBy({
    behavior: "smooth",
    left: 300
});
})

app.ports.scrollLeft.subscribe(function() {
  var target = document.getElementById("scrollCtr")
  target.scrollBy({
    behavior: "smooth",
    left: -300
});
})

app.ports.scrollR.subscribe(function(idx) {
  var target = document.getElementById("scrollCtr" + idx)
  target.scrollBy({
    behavior: "smooth",
    left: 300
});
})

app.ports.scrollL.subscribe(function(idx) {
  var target = document.getElementById("scrollCtr" + idx )
  target.scrollBy({
    behavior: "smooth",
    left: -300
});
})

app.ports.saveKey.subscribe(function (key) {
localStorage.setItem("contentsKey", key)
  app.ports.successSave.send("succees")
})

app.ports.getKey.subscribe(function () {
  var get = localStorage.getItem ("contentsKey") 
  if (window.location.hash == "#/mealRecord" ||  window.location.hash == "#/mealRecordM")
  {
    var list = get.split(',')
    // console.log(
  app.ports.receiveKey.send(get)
  // console.log({code : list[0], date : list[1]})
  } else {
  app.ports.receiveKey.send(get)
}
})
    
app.ports.saveId.subscribe(function (id) {
  if(id.code == undefined) {
    localStorage.setItem ("id", id)
  } else {
    localStorage.setItem ("id", JSON.stringify(id))
  }
  
  app.ports.successId.send("success")
})

app.ports.getId.subscribe(function () {
  // alert(1111)
  var get = localStorage.getItem("id")  
  if (get == null) {
    return false
  } else {
    var parse = JSON.parse (get)
   if (parse.code ==  undefined) {
    app.ports.receiveId.send (get)} 
    else if (parse.code == null) {
      return false;
    }
    else {
    app.ports.receiveId.send (parse)} 
  }
})

app.ports.removeId.subscribe(function() {
localStorage.removeItem("id")
})
app.ports.storeCache.subscribe(function(token) {
var t = JSON.stringify(token)
if (token === null) {
  localStorage.removeItem("token")
  localStorage.removeItem("refresh")
  location.replace("/")
    
} else {
  localStorage.setItem("token", t)
  setTimeout(() => {
    refreshFetch ()
  }, 1000);
}


app.ports.onStoreChange.send(token);

});



function refreshFetch () {
  let token = localStorage.getItem("token")
  var freshParse = JSON.parse(token)
  var refreshTokenHeader =  new Headers({
    "Content-Type": "application/json",
    "authorization": ("bearer " + freshParse.token)
  });

var tokenInit = 
{ method: 'GET',
headers: refreshTokenHeader,
mode: 'cors',
cache: 'default' };

fetch(url + 'auth/front/refresh',tokenInit)
  .then(response => {
    if(response.status == 401) {
      localStorage.removeItem("token")
      localStorage.removeItem("refresh")
      something();
      return location.replace("/")
    } else {
    return  response.json()
  }
  })
  .then(data => {
    var token = JSON.stringify(data)
    localStorage.setItem("refresh", token)
  
  })
}

app.ports.refreshFetchData.subscribe(function() {
    if (new Boolean (localStorage.getItem("refresh")) == true){
    let retoken =  localStorage.getItem ("refresh")
      let freshParse = JSON.parse(retoken)
        let refreshTokenHeader =  new Headers({
        "Content-Type": "application/json",
        "authorization": ("bearer " + freshParse.token)
      });

    let tokenInit = 
      { method: 'GET',
      headers: refreshTokenHeader,
      mode: 'cors',
      cache: 'default' };

      fetch(url + 'auth/front/token',tokenInit)
        .then(response => {
          if(response.status == 401) {
            localStorage.removeItem("token")
            localStorage.removeItem("refresh")
            something();
            return location.replace("/")
          } else {
          return  response.json()
        }
        })
        .then(data => {
          let token = JSON.stringify(data)
          localStorage.setItem ("token", token)
          app.ports.onStoreChange.send(data); 
          app.ports.onSucceesSession.send("complete")
          refreshFetch ();
        
        })
      }
      else {
        return;
      }
})




app.ports.toJs.subscribe(function(data) {
  console.log(data)
  var post = JSON.stringify(data);
  localStorage.setItem("addItem", post);
  app.ports.receive.send("ok")

});

app.ports.getfilter.subscribe(function() {
  var get = localStorage.getItem ("filter")
   app.ports.receiveFilter.send (
     JSON.parse(get)
     ); 
    
})

app.ports.filter.subscribe(function(filter) {
  localStorage.setItem ("filter", JSON.stringify(filter))
  app.ports.saveFilter.send ("success")
})




app.ports.getSomeFilter.subscribe(function() {
  var get = localStorage.getItem ("filter")
  localStorage.setItem ("filter", get)
  app.ports.getsaveFilter.send ("success")
    
})



app.ports.videoData.subscribe(function(data) {
 if (data == null || data == "") {
   jwplayer("myElement").remove();
 }
 else {
  if (data.pairing == "undefined" || ! Array.isArray (data.pairing))
  {
    jwplayer("myElement").setup (
      { "playlist" : data
      , autostart : true 
      }).on("playlistComplete", function () {
        app.ports.videoWatchComplete.send("complete")
      })
    }
  else {
    jwplayer("myElement").setup (
      { "playlist" : data.pairing
      , autostart : true 
      }).on("playlistComplete", function () {
        app.ports.videoWatchComplete.send("complete")
      })
  }
 }
});


app.ports.togetherDataList.subscribe(function(data) {
  if (data == null)
  {
    jwplayer("myElement" + data.id ).remove();
  }
  else { 
    jwplayer("myElement" + data.id ).setup (
    { "playlist" : data.pairing
    , autostart : true
    , "autoPause": {
      "viewability": true
    }
    }
  ).on("playlistComplete", function () {
    app.ports.videoWatchComplete.send("complete")
  })

  return false;
}
})
app.ports.removeJw.subscribe( function () {
  // jwplayer().remove()
  jwplayer().lenght == undefined ? "" : jwplayer().remove()
  // alert (document.getElementById('myElement'))
})
app.ports.showToast.subscribe(function (text) {
    var x = document.getElementById("webToast") || document.getElementById("appToast");
    x.className = "show";
    x.textContent = text
    setTimeout(function(){ x.className = x.className.replace("show", ""); }, 3000);
    return false;
})

app.ports.getscrollHeight.subscribe(function(data) {
  var heightValue = document.documentElement.scrollTop
  if (data){
    if (document.getElementById("calendarImg") ) {
      document.getElementById("calendarImg").style.top =  String(heightValue) + 'px' 
    
    }
    else {
    //   console.log (heightValue)
    // document.documentElement.style.top = '-' + String(heightValue) + 'px'
    // document.documentElement.style.position = "fixed"
  }
  }  else {
    if (document.getElementById("calendarImg")) {
      document.getElementById("calendarImg").style.top = ""
    } else
    {document.documentElement.style.position = ""
    document.documentElement.style.top = ''}
  }
})

app.ports.scrollControl.subscribe (function () {
  if (window.location.hash == "#/filterStep1" || window.location.hash == "#/makeExerciseEdit") {
    
    document.body.style.overflow = ""
  } 
  else if( window.location.hash == "#/myAccount") {
    // document.getElementById().ontouchend = (e) => {
    //   e.preventDefault();
      
    // }
   // console.log()
  //  document.documentElement.style.height = 0
  }
   else {
    document.body.style.overflow = "hidden"
  }
})

app.ports.mypageMenu.subscribe(function (val) {
  if (filter.indexOf( navigator.platform.toLowerCase() ) < 0 )
   { return ;
  } 
  else {
  if (document.getElementById("mypageMenu") == null || document.getElementById("scrollCtr") == null) {
    location.replace("#/myPage")
  } else {
  var item= document.getElementById("mypageMenu")
  var secItem = document.getElementById("scrollCtr")
  if (val) 
    {
      item.style.height == "110px"  ? item.style.height = "0px" : item.style.height = "110px" ;
      secItem.style.height == "110px"  ? secItem.style.height = "0px" : secItem.style.height = "110px" ;
    }
  else {
    secItem.style.height = "0px"
    item.style.height = "0px" } }
  }
})


app.ports.progressGo.subscribe(function () {
  let xxx = 0
  let bottom = 0
    var goProgress = setInterval(() => {
      xxx++
      document.getElementById("paperWeightProgress").value = xxx / 10
      
    if (xxx % 180 == 0) {
      bottom += 5
      document.getElementById("progressText").style.bottom = bottom + "rem"
    }
		if(xxx == 1000) {
      app.ports.progressComplete.send("complete")
      clearInterval(goProgress)
      
    } 
    }, 10);
})

app.ports.progressCalcuration.subscribe(function () {
  setTimeout(() => {
    app.ports.calcurationComplete.send ("complete")
  }, 3000);
})


app.ports.logoutpop.subscribe(function() {
var heightValue = document.documentElement.clientHeight
var checkDisplay = document.getElementById("logoutPop") || document.getElementById("mlogoutPop");
console.log(heightValue)
if (checkDisplay.className == "logoutShow")  {
  checkDisplay.class = checkDisplay.classList.remove("logoutShow");
  checkDisplay.style.height = 0 +'px'
}else {
  if (filter.indexOf( navigator.platform.toLowerCase() ) < 0 )
    {
      checkDisplay.className = "logoutShow";
      checkDisplay.style.height = "100vh"
      document.body.style.overflow = ""
    } else {
      checkDisplay.className = "logoutShow";
      checkDisplay.style.height = "100vh"
    }
}
})
  

var paymentCheck = false

app.ports.payment.subscribe(function (info) {
  alert ("가상결제이므로, 실제 결제는 이루어지지 않습니다.")

    IMP.init('imp85569385')
  IMP.request_pay({
    pay_method : 'card',
    merchant_uid : info.merchant_uid,
    name : info.name,
    amount : info.amount,
    buyer_email : info.buyer_email,
    buyer_name : info.buyer_name,
    digital : info.digital,
    m_redirect_url :window.location.origin + '#/gateProgress'
}, function(rsp) {
    if ( rsp.success ) {
        var msg = '결제가 완료되었습니다.';
        msg += '고유ID : ' + rsp.imp_uid;
        msg += '상점 거래ID : ' + rsp.merchant_uid;
        window.location.href=`${window.location.origin}?imp_uid=${rsp.imp_uid}&merchant_uid=${rsp.merchant_uid}&imp_success=true#/gateProgress`
        paymentCheck = true
    } else {
        var msg = '결제에 실패하였습니다';
        msg += ' : ' + rsp.error_msg;
        alert(msg)
    }
});
})

app.ports.mobilePaymentCheck.subscribe(function () {
  var origin = window.location.href.split("?")[1].split('&')
  var imp_uid = origin[0].split("=")[1]
  var merchant_id = origin[1].split("=")[1]
  var token = localStorage.getItem("token")
  var form = 
    {'imp_uid' : imp_uid  
    , 'merchant_uid': merchant_id}

  var headerInfo = new Headers ({
    "Content-Type": "application/json",
    "authorization": `bearer ${JSON.parse(token).token}`
  })
  var orderInfo = 
    { method: 'POST',
    headers: headerInfo,
    body: JSON.stringify(form)};
 fetch(url + 'front/orders/new', orderInfo)
  .then(res => {
    alert(res.status)
    if (res.status == 200) {
      window.location.href= `${window.location.origin}#/paperweightList`
    }
  })
  .then(data => {

  })
})


var slideIndex;
app.ports.slide.subscribe(function (index) {
  slideIndex = index

  })
if (document.getElementById('slide') !== null) {
  var timer,onlyOnce;
  onlyOnce = true;
  timer =  function(){
  if (onlyOnce) {
    app.ports.autoSlide.send ("autoSlideStart")
  }
  }
  window.setInterval(timer, 6000);

  document.addEventListener('transitionend', function(e){
    if (document.getElementById('slide') !== null) {
      if ((-(parseInt(slideIndex) * 100)+ "%")  == document.getElementById('slide').style.left) {
        e.preventDefault();
        app.ports.transitionCheck.send ("right")
      }

      if ("0%"  == document.getElementById('slide').style.left) {
        e.preventDefault();
        app.ports.transitionCheck.send ("left")
      }

      if (document.getElementById('stopInterval') !== null) {
        e.preventDefault();
        onlyOnce = false
        setTimeout(() => {
          app.ports.transitionCheck.send ("remove")
          onlyOnce = true
        }, 30000);
      }
    }
  }, { capture: false })

        var startPageX, endPageX
        document.addEventListener('touchstart', function (e) {
          var touchposition = e.changedTouches[0]
          startPageX = touchposition.pageX
          onlyOnce = false
      
        })
        document.addEventListener('touchmove', function (e) {
          e.preventDefault;
        })
        document.addEventListener('touchend', function (e) {
          var touchposition = e.changedTouches[0]
          endPageX = touchposition.pageX
          if (startPageX - endPageX < 0) {
            app.ports.swipe.send("left")
          } else {
            app.ports.swipe.send("right")
          }
          setTimeout(() => {
            onlyOnce = true
          }, 30000);
        })
  }

      
      
    




  

app.ports.valueReset.subscribe(function (id) {
  // alert(document.getElementById(id +"after"))
  // document.getElementById(id +"before").value = ""
  // document.getElementById(id +"after").value = ""
})

app.ports.youtubeVideo.subscribe(function (videoId) {
  app.ports.hideThum.send(videoId)
  var iframes = document.querySelectorAll('iframe');
    for (var i = 0; i < iframes.length; i++) {
        iframes[i].parentNode.removeChild(iframes[i]);
    }
  var innerDiv = document.createElement('div');
  innerDiv.id = 'player';
  document.getElementById("playerHere" + videoId).appendChild(innerDiv)
  var player;
    player = new YT.Player('player', {
      height: '100%',
      width: '100%',
      videoId: videoId,
      events: {
        'onReady': onPlayerReady,
      }
    });
  function onPlayerReady(event) {
    event.target.playVideo();
  }




  document.addEventListener('touchmove', function(e)
{
  var dv = document.getElementById("playerHere" + videoId)
  var st = document.getElementById("searchHeight").scrollTop
  if ((dv.offsetTop + dv.offsetHeight) <= st ) {
    player.stopVideo();
  }
}, 
{ capture: false } )
})



  document.addEventListener('touchmove', function(e) {
    
     if (document.getElementById("searchHeight"))  {
      var scrTop = document.getElementById("searchHeight").scrollTop
      var scrH = document.getElementById("searchHeight").scrollHeight
      var scrofh = document.getElementById("searchHeight").offsetHeight
      var total = scrTop + scrofh >= scrH
      if (total) {
          app.ports.touch.send(scrH)
          console.log(scrH)
      }
     
    }
    else {
      // console.log ("get outout")
      return;
    }
  }, 
  { capture: false });

 

document.addEventListener('touchmove', function(e) {
  if (document.getElementById("noScrInput")){
    // document.body.setAttribute('style','overflow:hidden;');
  e.preventDefault();}
  
  else {
    // console.log ("get outout")
    return;
  }
}, { passive: false });

document.addEventListener('DOMContentLoaded', () => {
  const $navbarBurgers = Array.prototype.slice.call(document.querySelectorAll('.navbar-burger'), 0);

  if ($navbarBurgers.length > 0) {

  $navbarBurgers.forEach( el => {
      el.addEventListener('click', () => {
      if (document.getElementsByClassName("is-active")[0] == undefined){
      document.getElementById("expandMenu").setAttribute('class','is-active')
    
    }
      else {
        document.getElementById("expandMenu").removeAttribute('class','is-active')
        document.getElementById("expandMenu").setAttribute('class','navbar-menu yf_menu')
      }
    })
  });
  event.stopPropagation();
}




});registerServiceWorker();
