// Code for IRQ_TN predictive validity task

// Check if HIT is in preview mode
function checkPreview() {
   if (turk.previewMode) {
       alert ("Please accept this HIT to see more questions.");
       return false;
   }
   return true;
}


// preload a list of imageUrls.
// the optional options argument can include:
// - afterEach: a function to call after each image is loaded
// - afterAll: a function to call after all images are loaded
// - completed: an array of already loaded image URLs

function preload(imageUrls, options) {
    
    if (options.afterEach) {
        afterEach = options.afterEach
    } else {
        afterEach = function(x, y) { } // default: do nothing
    }
    
    if (options.afterAll) {
        afterAll = options.afterAll
    } else {
        afterAll = function(x) { } // default: do nothing
    }
    
    if (options.completed) {
        completed = options.completed.slice();
    } else {
        completed = [] // default: empty
    }
    
    
    if (imageUrls.length == 0) {
    return afterAll(completed);
  }
    
    var current = imageUrls[0],
        remaining = imageUrls.slice(1),
        afterEach,
        afterAll,
        completed;
    
    
    completed.push(current);
  
    var image = new Image();
    image.onload = (function(afterEach, afterAll, completed, remaining) {
        return function() { 
            
            afterEach(completed, remaining);
            preload(remaining, {afterEach: afterEach, afterAll: afterAll, completed: completed});  
        }
    })(afterEach, afterAll, completed, remaining);
    
  image.src = current;
}

var afterEach = function(completed, remaining) { 
    //$("#completed").text(completed.length);
    //$("#remaining").text(remaining.length);
}

var afterAll = function(x) { 
    $("#next").removeAttr("disabled").text("Continue")
}

preload(["http://www.stanford.edu/~wcwill/fixation.png",
        "http://www.stanford.edu/~wcwill/IAPS/blank.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/1052.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/1220.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/1274.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/2053.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/3160.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/3220.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/3530.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/3550.1.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/6212.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/6550.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/6838.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/8230.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/9042.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/9181.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/9250.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/9300.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/9320.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/9373.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/9584.jpg",
        "http://www.stanford.edu/~wcwill/IAPS/9592.jpg"],
        
        {
            afterEach: afterEach,
            afterAll: afterAll
        }
        );


// Detect user's browser
//
var BrowserDetect = {
  init: function () {
    this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
    this.version = this.searchVersion(navigator.userAgent)
      || this.searchVersion(navigator.appVersion)
      || "an unknown version";
    this.OS = this.searchString(this.dataOS) || "an unknown OS";
  },
  searchString: function (data) {
    for (var i=0;i<data.length;i++) {
      var dataString = data[i].string;
      var dataProp = data[i].prop;
      this.versionSearchString = data[i].versionSearch || data[i].identity;
      if (dataString) {
        if (dataString.indexOf(data[i].subString) != -1)
          return data[i].identity;
      }
      else if (dataProp)
        return data[i].identity;
    }
  },
  searchVersion: function (dataString) {
    var index = dataString.indexOf(this.versionSearchString);
    if (index == -1) return;
    return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
  },
  dataBrowser: [
    {
      string: navigator.userAgent,
      subString: "Chrome",
      identity: "Chrome"
    },
    {   string: navigator.userAgent,
      subString: "OmniWeb",
      versionSearch: "OmniWeb/",
      identity: "OmniWeb"
    },
    {
      string: navigator.vendor,
      subString: "Apple",
      identity: "Safari",
      versionSearch: "Version"
    },
    {
      prop: window.opera,
      identity: "Opera",
      versionSearch: "Version"
    },
    {
      string: navigator.vendor,
      subString: "iCab",
      identity: "iCab"
    },
    {
      string: navigator.vendor,
      subString: "KDE",
      identity: "Konqueror"
    },
    {
      string: navigator.userAgent,
      subString: "Firefox",
      identity: "Firefox"
    },
    {
      string: navigator.vendor,
      subString: "Camino",
      identity: "Camino"
    },
    {   // for newer Netscapes (6+)
      string: navigator.userAgent,
      subString: "Netscape",
      identity: "Netscape"
    },
    {
      string: navigator.userAgent,
      subString: "MSIE",
      identity: "Explorer",
      versionSearch: "MSIE"
    },
    {
      string: navigator.userAgent,
      subString: "Gecko",
      identity: "Mozilla",
      versionSearch: "rv"
    },
    {     // for older Netscapes (4-)
      string: navigator.userAgent,
      subString: "Mozilla",
      identity: "Netscape",
      versionSearch: "Mozilla"
    }
  ],
  dataOS : [
    {
      string: navigator.platform,
      subString: "Win",
      identity: "Windows"
    },
    {
      string: navigator.platform,
      subString: "Mac",
      identity: "Mac"
    },
    {
         string: navigator.userAgent,
         subString: "iPhone",
         identity: "iPhone/iPod"
      },
    {
      string: navigator.platform,
      subString: "Linux",
      identity: "Linux"
    }
  ]

};
BrowserDetect.init();


// Validate question responses
//

function ValidateDemo() {   
    if ($('#mTurkIDtag').val()!="" && $('#agetag').val()!="" && $('input[name="gender"]:checked').val()!=null && 
        $('input[name="ethnicity"]:checked').val()!=null 
        ) {
        return true;
    } else {
        alert ( "Please answer all questions." );
        return false;    
    }
}


function ValidateIRQ1() {   
    if ($('input[name="irq_tn1"]:checked').val()!=null && $('input[name="irq_tn2"]:checked').val()!=null && 
      $('input[name="irq_en1"]:checked').val()!=null && $('input[name="irq_en2"]:checked').val()!=null && 
      $('input[name="irq_tp1"]:checked').val()!=null && $('input[name="irq_tp2"]:checked').val()!=null && 
      $('input[name="irq_ep1"]:checked').val()!=null && $('input[name="irq_ep2"]:checked').val()!=null
        ) {
        return true;
    } else {
        alert ( "Please answer all questions." );
        return false;    
    }
}


function ValidateIRQ2() {   
    if ($('input[name="irq_tn3"]:checked').val()!=null && $('input[name="irq_tn4"]:checked').val()!=null && 
      $('input[name="irq_en3"]:checked').val()!=null && $('input[name="irq_en4"]:checked').val()!=null && 
      $('input[name="irq_tp3"]:checked').val()!=null && $('input[name="irq_tp4"]:checked').val()!=null && 
      $('input[name="irq_ep3"]:checked').val()!=null && $('input[name="irq_ep4"]:checked').val()!=null
        ) {
        return true;
    } else {
        alert ( "Please answer all questions." );
        return false;    
    }
}


function ValidateERQ() {   
    if ($('input[name="erq_r1"]:checked').val()!=null && $('input[name="erq_r3"]:checked').val()!=null && 
      $('input[name="erq_r5"]:checked').val()!=null && $('input[name="erq_r7"]:checked').val()!=null && 
      $('input[name="erq_r8"]:checked').val()!=null && $('input[name="erq_r10"]:checked').val()!=null && 
      $('input[name="erq_s2"]:checked').val()!=null && $('input[name="erq_s4"]:checked').val()!=null &&
      $('input[name="erq_s6"]:checked').val()!=null && $('input[name="erq_s9"]:checked').val()!=null
        ) {
        return true;
    } else {
        alert ( "Please answer all questions." );
        return false;    
    }
}


function ValidateBFI_E() {   
    if ($('input[name="bfi_e1"]:checked').val()!=null && $('input[name="bfi_e6"]:checked').val()!=null && 
      $('input[name="bfi_e11"]:checked').val()!=null && $('input[name="bfi_e16"]:checked').val()!=null && 
      $('input[name="bfi_e21"]:checked').val()!=null && $('input[name="bfi_e26"]:checked').val()!=null && 
      $('input[name="bfi_e31"]:checked').val()!=null && $('input[name="bfi_e36"]:checked').val()!=null
        ) {
        return true;
    } else {
        alert ( "Please answer all questions." );
        return false;    
    }
}


function ValidateFunnel1() {   
    if ($('#task3_aboutTag').val()!=""
        ) {
        return true;
    } else {
        alert ( "Please answer all questions." );
        return false;    
    }
}


function ValidateFunnel2() {   
    if ($('#task3_suspectTag').val()!=""
        ) {
        return true;
    } else {
        alert ( "Please answer all questions." );
        return false;    
    }
}


function ValidateFunnel3() {   
    if ($('input[name="task3_check"]:checked').val()!=null && $('#task3_beliefTag').val()!=""
        ) {
        return true;
    } else {
        alert ( "Please answer all questions." );
        return false;    
    }
}


// Show slide workhorse function
//
function showSlide(id) {

	$(".slide").hide();

	$("#"+id).show();

}


// Random number generator
//
function random(a,b) {
  if (typeof b == "undefined") {
    a = a || 2;
    return Math.floor(Math.random()*a);
  } else {
    return Math.floor(Math.random()*(b-a+1)) + a;
  }
}

Array.prototype.random = function() {
  return this[random(this.length)];
}

Array.prototype.shuffle = function(){
  for(var j, x, i = this.length; i; j = parseInt(Math.random() * i), x = this[--i], this[i] = this[j], this[j] = x) {}
  return this;
}


// taskSection key bindings
//
var task_allKeyBindings = [{"1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7}],

    unshuffledTrialOrder0 = [
    {n: "1", pic: "1052"},
    {n: "2", pic: "1220"},
    {n: "3", pic: "1274"},
    {n: "4", pic: "2053"},
    {n: "5", pic: "3160"},
    {n: "6", pic: "3220"}, 
    {n: "7", pic: "3530"},
    {n: "8", pic: "3550.1"}, 
    {n: "9", pic: "6212"},
    {n: "10", pic: "6550"},
    {n: "11", pic: "6838"},
    {n: "12", pic: "8230"},
    {n: "13", pic: "9042"},
    {n: "14", pic: "9181"},
    {n: "15", pic: "9250"},
    {n: "16", pic: "9300"},
    {n: "17", pic: "9320"},
    {n: "18", pic: "9373"},
    {n: "19", pic: "9584"},
    {n: "20", pic: "9592"}
    ],


    taskKeyBindings = task_allKeyBindings.random();
    myTrialOrder0 = unshuffledTrialOrder0.shuffle();
    

//Show start page
//
showSlide("start");


// taskSection task
//
var taskSection = {

	trials: myTrialOrder0,

	keyBindings: taskKeyBindings,

  reactionTimeArray: [],
  responseArray: [],
  nArray: [],
  startTime: 0,
  endTime: 0,
  stageInExperiment: 0,
  numDigits: 0,
  userResp: 0,

	data: [],

  instructions1: function() {
    showSlide("instructions1");
  },

  instructions2: function() {
    showSlide("instructions2");
  },

  instructions3: function() {
    showSlide("instructions3");
  },

  instructions4: function() {
    showSlide("instructions4");
  },

  pause: function() {
    showSlide("pause");
  },


	next: function() {

		var trial = taskSection.trials.shift();


		if (typeof trial == "undefined" && taskSection.stageInExperiment==0) {
      	      taskSection.trials = myTrialOrder0;
                  taskSection.stageInExperiment=1;
                  return taskSection.pause();
    }
    

    var keyPressHandler = function(event) {

      var keyCode = event.which;
      
      if (keyCode != 49 && keyCode != 50 && keyCode != 51 && keyCode != 52 && keyCode != 53 && keyCode != 54 && keyCode != 55 && keyCode != "timeout") {

        $(document).one("keydown", keyPressHandler);

      } else {

        taskSection.endTime = (new Date()).getTime();

        if (keyCode == 49) { // 1
          taskSection.userResp = 1;
        } else if (keyCode == 50) { // 2
          taskSection.userResp = 2;
        } else if (keyCode == 51) { // 3
          taskSection.userResp = 3;
        } else if (keyCode == 52) { // 4
          taskSection.userResp = 4;
        } else if (keyCode == 53) { // 5
          taskSection.userResp = 5;
        } else if (keyCode == 54) { // 6
          taskSection.userResp = 6;
        } else if (keyCode == 55) { // 7
          taskSection.userResp = 7;
        } 

        pictureElement.setAttribute("src", "");
        $("#nElement").html(taskSection.userResp);

          data = {
                n: trial.n,
                pic: trial.pic,
                rt: taskSection.endTime - taskSection.startTime,
                response: taskSection.userResp,
                browser: BrowserDetect.browser
              };
              
          taskSection.responseArray.push(taskSection.userResp);
          taskSection.reactionTimeArray.push(taskSection.endTime - taskSection.startTime);              
          taskSection.nArray.push(trial.n); 
          taskSection.data.push(data);

          setTimeout(function(){ taskSection.next()} ,2000);
          
      }
    };

    showSlide("stage");
    $("#nElement").html("");
    pictureElement = document.getElementById("imageElement");
    
    setTimeout(function(){ pictureElement.setAttribute("src", "http://www.stanford.edu/~wcwill/IAPS/" + trial.pic + ".jpg");
                           $("#nElement").html("Rate how unpleasant you find the image from 1 (Not at all) to 4 (Unpleasant) to 7 (Extremely unpleasant)");
                           taskSection.startTime = (new Date()).getTime();
                           $(document).one("keydown", keyPressHandler);
                           } ,2000);
	}
}


// questionSection task
//
var questionSection = {

  reactionTimeArray: [],
  responseArray: [],
  qArray: [],
  startTime: 0,
  stageInExperiment: 0,


  data: [],

  end: function() {
    
    questionSection.comments = $('textarea[id="commentsTag"]').val();

    data: [],
    data = {
                  mTurkID: questionSection.mTurkID,
                  age: questionSection.age,
                  gender: questionSection.gender,
                  ethnicity: questionSection.ethnicity,

                  irq_tn1: questionSection.irq_tn1,
                  irq_tn2: questionSection.irq_tn2,
                  irq_en1: questionSection.irq_en1,
                  irq_en2: questionSection.irq_en2,
                  irq_tp1: questionSection.irq_tp1,
                  irq_tp2: questionSection.irq_tp2,
                  irq_ep1: questionSection.irq_ep1,
                  irq_ep2: questionSection.irq_ep2,
                  irq_tn3: questionSection.irq_tn3,
                  irq_tn4: questionSection.irq_tn4,
                  irq_en3: questionSection.irq_en3,
                  irq_en4: questionSection.irq_en4,
                  irq_tp3: questionSection.irq_tp3,
                  irq_tp4: questionSection.irq_tp4,
                  irq_ep3: questionSection.irq_ep3,
                  irq_ep4: questionSection.irq_ep4,
                  irq_tn_total: questionSection.irq_tn_total,
                  irq_tp_total: questionSection.irq_tp_total,
                  irq_en_total: questionSection.irq_en_total,
                  irq_ep_total: questionSection.irq_ep_total,
                  irq_total: questionSection.irq_total,

                  erq_r1: questionSection.erq_r1,
                  erq_r3: questionSection.erq_r3,
                  erq_r5: questionSection.erq_r5,
                  erq_r7: questionSection.erq_r7,
                  erq_r8: questionSection.erq_r8,
                  erq_r10: questionSection.erq_r10,
                  erq_s2: questionSection.erq_s2,
                  erq_s4: questionSection.erq_s4,
                  erq_s6: questionSection.erq_s6,
                  erq_s9: questionSection.erq_s9,
                  erq_r_total: questionSection.erq_r_total,
                  erq_s_total: questionSection.erq_s_total,

                  bfi_e1: questionSection.bfi_e1,
                  bfi_e6: questionSection.bfi_e6,
                  bfi_e11: questionSection.bfi_e11,
                  bfi_e16: questionSection.bfi_e16,
                  bfi_e21: questionSection.bfi_e21,
                  bfi_e26: questionSection.bfi_e26,
                  bfi_e31: questionSection.bfi_e31,
                  bfi_e36: questionSection.bfi_e36,
                  bfi_e_total: questionSection.bfi_e_total,

                  task3_about: questionSection.task3_about,
                  task3_suspect: questionSection.task3_suspect,
                  task3_check: questionSection.task3_check,
                  task3_belief: questionSection.task3_belief,

                  comments: questionSection.comments

              };
    
    questionSection.data.push(data);
    showSlide("finished");
    setTimeout(function() { turk.submit({
      "taskSection": taskSection,
      "questionSection": questionSection,
      "choiceSection": choiceSection })
    }, 1500);
  },


  postQ1: function() {
    showSlide("postQ1");
      },

  loadscreen: function() {
    showSlide("loadscreen");
    $("#loadElement").html("Searching")

    setTimeout(function() {$("#loadElement").html("Searching.")}, 1000);
    setTimeout(function() {$("#loadElement").html("Searching..")}, 2000);
    setTimeout(function() {$("#loadElement").html("Searching...")}, 3000);
    setTimeout(function() {$("#loadElement").html("Searching.")}, 4000);
    setTimeout(function() {$("#loadElement").html("Searching..")}, 5000);
    setTimeout(function() {$("#loadElement").html("Searching...")}, 6000);
    setTimeout(function() {$("#loadElement").html("Searching.")}, 7000);
    setTimeout(function() {$("#loadElement").html("Searching..")}, 8000);
    setTimeout(function() {$("#loadElement").html("Searching...")}, 9000);
    setTimeout(function() {$("#loadElement").html("Searching.")}, 10000);
    setTimeout(function() {$("#loadElement").html("User found!")}, 10380);
    setTimeout(function() {$("#loadElement").html("Connecting.")}, 14000);
    setTimeout(function() {$("#loadElement").html("Connecting..")}, 15000);
    setTimeout(function() {$("#loadElement").html("Connecting...")}, 16000);
    setTimeout(function() {$("#loadElement").html("Connecting.")}, 17000);
    setTimeout(function() {$("#loadElement").html("Connecting..")}, 18000);
    setTimeout(function() {$("#loadElement").html("Connecting...")}, 19000);
    setTimeout(function() {$("#loadElement").html("Connecting.")}, 20000);
    setTimeout(function() {$("#loadElement").html("Connecting..")}, 21000);
    setTimeout(function() {$("#loadElement").html("Connected!")}, 21320);
    setTimeout(function() {$("#loadElement").html("Loading.")}, 26000);
    setTimeout(function() {$("#loadElement").html("Loading..")}, 27000);
    setTimeout(function() {$("#loadElement").html("Loading...")}, 28000);
    setTimeout(function() {showSlide("choiceinfo1");}, 30600);

  },

  choiceinfo1: function() {
    showSlide("choiceinfo1");
      },

  choiceinfo2: function() {
    showSlide("choiceinfo2");
      },

  choiceinfo3: function() {
    showSlide("choiceinfo3");
      },

  choiceinfo4: function() {
    showSlide("choiceinfo4");
      },

  demo: function() {
    showSlide("demo");
      },

  IRQ1: function() {
    questionSection.mTurkID = $('input[name="mTurkID"]').val();
    questionSection.age = $('input[name="age"]').val();
    questionSection.gender = $('input[name="gender"]:checked').val();
    questionSection.ethnicity = $('input[name="ethnicity"]:checked').val();
    showSlide("IRQ1");
      },

  IRQ2: function() {
    questionSection.irq_tn1 = $('input[name="irq_tn1"]:checked').val();
    questionSection.irq_tn2 = $('input[name="irq_tn2"]:checked').val();
    questionSection.irq_en1 = $('input[name="irq_en1"]:checked').val();
    questionSection.irq_en2 = $('input[name="irq_en2"]:checked').val();
    questionSection.irq_tp1 = $('input[name="irq_tp1"]:checked').val();
    questionSection.irq_tp2 = $('input[name="irq_tp2"]:checked').val();
    questionSection.irq_ep1 = $('input[name="irq_ep1"]:checked').val();
    questionSection.irq_ep2 = $('input[name="irq_ep2"]:checked').val();
    showSlide("IRQ2");
      },

  ERQ: function() {
    questionSection.irq_tn3 = $('input[name="irq_tn3"]:checked').val();
    questionSection.irq_tn4 = $('input[name="irq_tn4"]:checked').val();
    questionSection.irq_en3 = $('input[name="irq_en3"]:checked').val();
    questionSection.irq_en4 = $('input[name="irq_en4"]:checked').val();
    questionSection.irq_tp3 = $('input[name="irq_tp3"]:checked').val();
    questionSection.irq_tp4 = $('input[name="irq_tp4"]:checked').val();
    questionSection.irq_ep3 = $('input[name="irq_ep3"]:checked').val();
    questionSection.irq_ep4 = $('input[name="irq_ep4"]:checked').val();
    questionSection.irq_tn_total = +questionSection.irq_tn1 + +questionSection.irq_tn2 + +questionSection.irq_tn3 + +questionSection.irq_tn4;
    questionSection.irq_tn_total = questionSection.irq_tn_total+'';
    questionSection.irq_en_total = +questionSection.irq_en1 + +questionSection.irq_en2 + +questionSection.irq_en3 + +questionSection.irq_en4;
    questionSection.irq_en_total = questionSection.irq_en_total+'';
    questionSection.irq_tp_total = +questionSection.irq_tp1 + +questionSection.irq_tp2 + +questionSection.irq_tp3 + +questionSection.irq_tp4;
    questionSection.irq_tp_total = questionSection.irq_tp_total+'';
    questionSection.irq_ep_total = +questionSection.irq_ep1 + +questionSection.irq_ep2 + +questionSection.irq_ep3 + +questionSection.irq_ep4;
    questionSection.irq_ep_total = questionSection.irq_ep_total+'';
    questionSection.irq_total = +questionSection.irq_tn_total + +questionSection.irq_en_total + +questionSection.irq_tp_total + +questionSection.irq_ep_total;
    questionSection.irq_total = questionSection.irq_total+'';
    showSlide("ERQ");
      },

  BFI_E: function() {
    questionSection.erq_r1 = $('input[name="erq_r1"]:checked').val();
    questionSection.erq_s2 = $('input[name="erq_s2"]:checked').val();
    questionSection.erq_r3 = $('input[name="erq_r3"]:checked').val();
    questionSection.erq_s4 = $('input[name="erq_s4"]:checked').val();
    questionSection.erq_r5 = $('input[name="erq_r5"]:checked').val();
    questionSection.erq_s6 = $('input[name="erq_s6"]:checked').val();
    questionSection.erq_r7 = $('input[name="erq_r7"]:checked').val();
    questionSection.erq_r8 = $('input[name="erq_r8"]:checked').val();
    questionSection.erq_s9 = $('input[name="erq_s9"]:checked').val();
    questionSection.erq_r10 = $('input[name="erq_r10"]:checked').val();
    questionSection.erq_r_total = +questionSection.erq_r1 + +questionSection.erq_r3 + +questionSection.erq_r5 + +questionSection.erq_r7 + +questionSection.erq_r8  + +questionSection.erq_r10;
    questionSection.erq_r_total = questionSection.erq_r_total+'';
    questionSection.erq_s_total = +questionSection.erq_s2 + +questionSection.erq_s4 + +questionSection.erq_s6 + +questionSection.erq_s9;
    questionSection.erq_s_total = questionSection.erq_s_total+'';
    showSlide("BFI_E");
      },

  funnel1: function() {
    questionSection.bfi_e1 = $('input[name="bfi_e1"]:checked').val();
    questionSection.bfi_e6 = $('input[name="bfi_e6"]:checked').val();
    questionSection.bfi_e11 = $('input[name="bfi_e11"]:checked').val();
    questionSection.bfi_e16 = $('input[name="bfi_e16"]:checked').val();
    questionSection.bfi_e21 = $('input[name="bfi_e21"]:checked').val();
    questionSection.bfi_e26 = $('input[name="bfi_e26"]:checked').val();
    questionSection.bfi_e31 = $('input[name="bfi_e31"]:checked').val();
    questionSection.bfi_e36 = $('input[name="bfi_e36"]:checked').val();
    questionSection.bfi_e_total = +questionSection.bfi_e1 + +questionSection.bfi_e6 + +questionSection.bfi_e11 + +questionSection.bfi_e16 + +questionSection.bfi_e21 + +questionSection.bfi_e26 + +questionSection.bfi_e31 + +questionSection.bfi_e36;
    questionSection.bfi_e_total = questionSection.bfi_e_total+'';
    showSlide("funnel1");
      },

  funnel2: function() {
    questionSection.task3_about = $('textarea[id="task3_aboutTag"]').val();
    showSlide("funnel2");
      },

  funnel3: function() {
    questionSection.task3_suspect = $('textarea[id="task3_suspectTag"]').val();
    showSlide("funnel3");
      },

  debrief1: function() {
    questionSection.task3_check = $('input[name="task3_check"]:checked').val();
    questionSection.task3_belief = $('textarea[id="task3_beliefTag"]').val();
    showSlide("debrief1");
      },

  debrief2: function() {
    showSlide("debrief2");
      },

  debrief3: function() {
    showSlide("debrief3");
      }

}


// choiceSection keybindings
//
var choice_allKeyBindings = [
  {"m": "no_person", "z": "person"},
  {"m": "person", "z": "no_person"} ],

    unshuffledChoiceOrder0 = ["choice"],

    cKeyBindings = choice_allKeyBindings.random();
    myChoiceOrder0 = unshuffledChoiceOrder0.shuffle();
    pOdd = (cKeyBindings["m"] == "no_person");


// choiceSection task
//
var choiceSection = {

  trials: myChoiceOrder0,

  keyBindings: cKeyBindings,

  reactionTimeArray: [],
  responseArray: [],
  qArray: [],
  startTime: 0,
  stageInExperiment: 0,

  data: [],


  postchoice: function() {
    showSlide("postchoice");
      },


  next: function() {

      var trial = choiceSection.trials.shift();

      if (typeof trial == "undefined" && choiceSection.stageInExperiment==0) {
              return choiceSection.postchoice();
      }

    showSlide("choice");
    $("#no_person-key").html(pOdd ? "M" : "Z");
    $("#person-key").html(pOdd ? "Z" : "M");

    choiceSection.startTime = (new Date()).getTime();


    var keyPressHandler = function(event) {

      var keyCode = event.which;
      
      if (keyCode != 90 && keyCode != 77 && keyCode != "timeout") {

        $(document).one("keydown", keyPressHandler);

      } else {

        var endTime = (new Date()).getTime(),
        
        key = (keyCode == 77) ? "m" : "z",
        userResp = choiceSection.keyBindings[key],
        
        data = {
                q: trial,
                rt: endTime - choiceSection.startTime,
                response: userResp,
                browser: BrowserDetect.browser
              };
              

        choiceSection.responseArray.push(userResp);
        choiceSection.reactionTimeArray.push(endTime - choiceSection.startTime);
              
        choiceSection.data.push(data);

        setTimeout(choiceSection.next, 500);
      }
    };

    $(document).one("keydown", keyPressHandler);

  }
}

