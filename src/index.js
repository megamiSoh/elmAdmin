import './css/bulma.css';
import './main.css';
import './css/all.min.css'
import './css/app.css'
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

if(!!document.createRange) {
  document.getSelection().removeAllRanges();
}

const url ='http://13.209.49.169:4000/api/v1/'
var filter = "win16|win32|win64|mac|macintel"; 
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
        }
    };
})();



var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags : flags,
  // mobileCheck : mobileCheck
  
});




app.ports.sendData.subscribe(function () {
  var val = localStorage.getItem("addItem")
  if(val != undefined) {
    app.ports.receiveData.send(JSON.parse(val))
    
  }
 
});


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
  app.ports.receiveKey.send(get)
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
  alert(1111)
  var get = localStorage.getItem("id")  
  var parse = JSON.parse (get)
  console.log (parse.code == undefined)
   if (parse.code ==  undefined) {
    app.ports.receiveId.send (get)} 
    else{
    app.ports.receiveId.send (parse)} 
})

app.ports.removeId.subscribe(function() {
localStorage.removeItem("id")
})
app.ports.storeCache.subscribe(function(token) {
var t = JSON.stringify(token)
if (token === null) {
  localStorage.removeItem("token")
  localStorage.removeItem ("refresh")
  window.location.href = "/"
  location.reload()
    
} else {
  localStorage.setItem("token", t)
}
app.ports.onStoreChange.send(token);

});


app.ports.secRefreshFetch.subscribe(function() {
  var retoken = localStorage.getItem ("refresh")
    var freshParse = JSON.parse(retoken)
  // var getUrl = "auth/front/show"
if (retoken == null) {
  localStorage.removeItem("token")
  localStorage.removeItem("refresh")
  window.location.href = "/"
  location.reload()
} else {

  var retoken = localStorage.getItem ("refresh")
    var freshParse = JSON.parse(retoken)
  // var getUrl = "auth/front/show"
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
        window.location.href = "/"
        return location.reload()
      } else {
      return  response.json()
    }
    })
    .then(data => {
      var token = JSON.stringify(data)
      
      localStorage.setItem ("token", token)
      // alert("tokenSaveSuccess")
      var tokenHeader = new Headers ({
        "Content-Type": "application/json",
        "authorization": ("bearer " + data.token)
      })

      var refreshInit = 
      { method: 'GET',
      headers: tokenHeader,
      mode: 'cors',
      cache: 'default' };    
      app.ports.onStoreChange.send(data);
      // fetch(url + getUrl, refreshInit)
      //   .then(response => 
      //     console.log(response))
      //   .then(() => {
      //       // console.log(data.status)
      //         var tokenReceive = localStorage.getItem ("token")
      //         app.ports.onStoreChange.send(JSON.parse(tokenReceive)); 
      //         app.ports.onSucceesSession.send("complete")
      //         // location.reload()
      //   })
      //   .catch(error => alert( "showerror" + error))
        
        setTimeout(() => {
          fetch(url + 'auth/front/refresh',refreshInit)
          .then(response => response.json()
          )
          .then(refreshT => {
            // alert("refreshToken succeess")
            var refresh = JSON.stringify(refreshT)
            localStorage.setItem ("refresh", refresh)
          })
          .catch(error => alert(error))
        }, 30000);
    })
    .catch(error => console.log(error))

  }

    
})


app.ports.refreshFetchData.subscribe(function() {
  var token = localStorage.getItem("token")
  var parse = JSON.parse(token)
   
    var myHeaders =  new Headers({
      "Content-Type": "application/json",
      "authorization": ("bearer " + parse.token)
    });
  
  var myInit = 
    { method: 'GET',
    headers: myHeaders,
    mode: 'cors',
    cache: 'default' };
    app.ports.onStoreChange.send(parse);
 setTimeout(function() {
    fetch(url + 'auth/front/refresh',myInit)
    .then(response => {
     
      return  response.json()
    })
    .then(data => {
      // alert(JSON.stringify(data))
      var refresh = JSON.stringify(data)
     localStorage.setItem ("refresh", refresh)
    
    })
    .catch(error => 
     alert(error)
     
      )
  }, 30000)
 
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
  alert("call")
  const element = jwplayer("myElement")

    if (data.pairing == "undefined" || ! Array.isArray (data.pairing))
  	{
      jwplayer("myElement").setup(
        {"playlist" : data 
        , autostart : true }
      ).on("ready")
      app.ports.videoSuccess.send(true)
    }
  else {
    {
      
      jwplayer("myElement").setup(
        {"playlist" : data.pairing
        , autostart : true }
      ).on("ready")
      app.ports.videoSuccess.send(true)
    }
  }
});


app.ports.togetherDataList.subscribe(function(data) {
  console.log(data)
  if (data == null)
  {

  }
  else { 
    jwplayer("myElement" + data.id ).setup(
    { "playlist" : data.pairing
    , autostart : true 
    }
  ).on("ready")
}
    
 
})
app.ports.showToast.subscribe(function (text) {
    var x = document.getElementById("webToast") || document.getElementById("appToast");
    x.className = "show";
    x.textContent = text
    setTimeout(function(){ x.className = x.className.replace("show", ""); }, 3000);
})

app.ports.blur.subscribe(function() {
  var id = document.getElementById("keyboardBlur")
  id.blur();
})

// app.ports.expand.subscribe(function() {
//   alert(111)
//   // const $navbarBurgers = Array.prototype.slice.call(document.querySelectorAll('.navbar-burger'), 0);

//   // // Check if there are any navbar burgers
//   // if ($navbarBurgers.length > 0) {

//   // // Add a click event on each of them
//   // $navbarBurgers.forEach( el => {
//   //     el.addEventListener('click', () => {

//   //     // Get the target from the "data-target" attribute
//   //     const target = el.dataset.target;
//       const $target = document.getElementById("expandMenu");

//       // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
//       el.classList.toggle('is-active');
//       $target.classList.toggle('is-active');

      

//   //     });
//   // });
// // }
// })
document.addEventListener('DOMContentLoaded', () => {
  // Get all "navbar-burger" elements
  const $navbarBurgers = Array.prototype.slice.call(document.querySelectorAll('.navbar-burger'), 0);

  // Check if there are any navbar burgers
  if ($navbarBurgers.length > 0) {

  // Add a click event on each of them
  $navbarBurgers.forEach( el => {
      el.addEventListener('click', () => {
        console.log()
      if (document.getElementsByClassName("is-active")[0] == undefined){
      document.getElementById("expandMenu").setAttribute('class','is-active')
    
    }
      else {
        document.getElementById("expandMenu").removeAttribute('class','is-active')
        document.getElementById("expandMenu").setAttribute('class','navbar-menu yf_menu')
      }
    })
  });
}

// app.ports.confirmDialog.subscribe(function () {

// })

});registerServiceWorker();
