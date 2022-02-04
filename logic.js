function httpGetAsync(theUrl, callback) {
  var xmlHttp = new XMLHttpRequest();
  xmlHttp.onreadystatechange = function () {
    if (xmlHttp.readyState == 4 && xmlHttp.status == 200)
      callback(xmlHttp.responseText);
  };
  xmlHttp.open("GET", theUrl, true); // true for asynchronous
  xmlHttp.send(null);
}

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function doModify(elem) {
  console.log('Taking a break...');
  var where = null, when = null, what = null;
  for (attr in elem) {
    switch (attr) {
      case "Position":
        console.log(elem[attr]);
        where = elem[attr]
        break;
      case "Time":
        console.log(elem[attr]);
        when = elem[attr];
        break;
      case "Content":
        console.log(elem[attr]);
        what = elem[attr];
        for (val in what) {
          switch (val) {
            case "Words":
              const para = document.createElement("p");
              const node = document.createTextNode(what[val].concat());
              para.appendChild(node);
              what = para;
              break;
            case "Url":
              const paraU = document.createElement("p");
              const anchor = document.createElement("a");
              anchor.setAttribute('href', what[val]);
              anchor.innerHTML = "a dummy Url";
              paraU.appendChild(anchor);
              what = paraU;
              break;
            case "Video":
              const paraV = document.createElement("div");
              const vidV = document.createElement("video");
              vidV.src = what[val]["Url"];
              vidV.autoplay = true;
              vidV.controls = true;
              vidV.muted = true;
              paraV.appendChild(vidV);
              what = paraV;
              break;
            case "Opt":
              const form0 = document.createElement("form");
              const sel0 = document.createElement("select");
              for (optN in what[val]) {
                optE = document.createElement("option");
                console.log(optN);
                optE.value = what[val][optN];
                optE.innerHTML = what[val][optN];
                sel0.appendChild(optE);
              }
              form0.appendChild(sel0);
              form0.appendChild(document.createElement("input"));
              what = form0;
              break
            default:
              console.log("bad" + val);
          }
        }
        break;
      default:
        console.log("bad" + attr);
    }
  }
  var howLong = 0;
  for (dnom in when) {
    switch (dnom) {
      case "Seconds":
        console.log(when[dnom])
        howLong = when[dnom] * 1000;
        break;
      default:
        console.log("bad");
    }
  }
  document.body.appendChild(what);
  return howLong;
}

async function callback(response) {
  const parsed = JSON.parse(response);
  for (resp in parsed) {
    switch (resp) {
      case "One":
        console.log(parsed[resp]);
        var i = 0;
        while (i < parsed[resp].length) {
          for (val in parsed[resp][i]) {
            switch (val) {
              case "Element":
                console.log(val);
                var elem = parsed[resp][i][val];
                var howLong = doModify(elem);
                await sleep(howLong);
                console.log(howLong + ' milli seconds later, showing sleep in a loop...');
                break;
              default:
                console.log("bad" + val);
            }
          }
          i++;
        }
        break;
      case "Many":
        break;
      default:
        console.log("bad" + resp);
    }
  }
}

httpGetAsync("http://192.168.18.198/assets/sample.json", callback);