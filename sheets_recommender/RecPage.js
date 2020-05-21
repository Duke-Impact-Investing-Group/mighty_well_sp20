
//  <script src="https://code.jquery.com/jquery-1.6.4.js"></script>
    
    const CATALOG = {
        "PICCPerfect": ["https://mighty-well.com/products/picc-line-cover-piccperfect-mighty-well?_pos=5&_sid=db99fc554&_ss=r", "https://blog.mighty-well.com/2017/07/adapting-to-the-picc-life/"],
        "MedPlanner":  ["https://mighty-well.com/collections/all/products/mighty-medplanner-medical-organizer","https://blog.mighty-well.com/2019/06/trying-out-the-mighty-medplanner-your-new-medical-organizer/"],
        "Mighty Pack": ["https://mighty-well.com/collections/all/products/medical-iv-infusion-backpack-for-patients","https://blog.mighty-well.com/2018/12/travel-without-worries-with-the-mighty-pack-the-best-backpack-for-friends-in-the-fight/"],
        "Mighty Wrap": ["https://mighty-well.com/collections/all/products/the-mighty-jacket","https://blog.mighty-well.com/2017/06/learning-to-love-your-tube/"],
        "Undefeated Wear":["https://mighty-well.com/collections/all/products/gift-bundle-mighty-medplanner","https://blog.mighty-well.com/2019/11/mighty-well-fall-photoshoot/"]
    }

    const scriptURL = "https://script.google.com/macros/s/AKfycbxx4_QDeXS7AqXlU9WkYvDTvdY_dYHUMbZ-YdQK4SslU54ihug/exec"
    const form = document.forms[0]
    const proxyurl = "https://cors-anywhere.herokuapp.com/";


    form.addEventListener("submit", e => {
        e.preventDefault()
        fetch(scriptURL, {
            method: "POST",
            body: new FormData(form)
            })
            .then(response => console.log("Success!", response))
            .catch(error => console.error("Error!", error.message))
        })

    var currentTab = 0; // Current tab is set to be the first tab (0)
    var productRec = [];
    var gridTable = new Object;
    
    showTab(currentTab); // Display the current tab
        
    function showTab(n) {
        // This function will display the specified tab of the form ...
        var x = document.getElementsByClassName("tab");
        x[n].style.display = "block";
        // ... and fix the Previous/Next buttons:
        if(n == x.length-1){
            document.getElementById("prevBtn").style.display = "none";
            document.getElementById("nextBtn").style.display = "none";
            document.getElementById("finalsub").style.display = "none";
            document.getElementById("startOverBtn").style.display="inline";
        }
        else{
        document.getElementById("startOverBtn").style.display="none";

        if (n == 0) {
            document.getElementById("prevBtn").style.display = "none";
        } else {
            document.getElementById("prevBtn").style.display = "inline";
        }
        if (n == (x.length - 2)) {
            document.getElementById("finalsub").style.display = "inline";
            document.getElementById("nextBtn").style.display = "none";
        } else {
            document.getElementById("nextBtn").style.display = "inline";
            document.getElementById("finalsub").style.display = "none";

        }}
        // ... and run a function that displays the correct step indicator:
        fixStepIndicator(n)
    }

    function nextPrev(n) {
        // This function will figure out which tab to display
        var x = document.getElementsByClassName("tab");
        // Exit the function if any field in the current tab is invalid:
        if (n == 1 && !validateForm()) return false;
        // Hide the current tab:
        x[currentTab].style.display = "none";
        // Increase or decrease the current tab by 1:
        currentTab = currentTab + n;
        // if you have reached the end of the form... :
        if (currentTab >= x.length) {
            document.getElementById("demo").innerHTML = "check";
            return false;
        }
    
        if(currentTab==x.length-1){ 
        calculateProduct();
        buildGridTable();
        createProductPage();
        displayProductPage();
            //console.log("finished building gridtable")
            //console.log(gridTable);
            //createProductPage();
            //console.log("finished building product page")
            //displayProductPage();
        }

        // Otherwise, display the correct tab:
        showTab(currentTab);
    }

    function validateForm() {
        // This function deals with validation of the form fields
        var x, y, i, valid = false;
        x = document.getElementsByClassName("tab");
        y = x[currentTab].getElementsByTagName("input");
        for (i = 0; i < y.length; i++) {
            if (y[i].checked) {
            valid = true;
            break;
            }
        }
        if (valid) {
            document.getElementsByClassName("step")[currentTab].className += " finish";
        }
        return valid; // return the valid status
    }

    function fixStepIndicator(n) {
        // This function removes the "active" class of all steps...
        var i, x = document.getElementsByClassName("step");
        for (i = 0; i < x.length+1; i++) {
            x[i].className = x[i].className.replace(" active", "");
        }
        //... and adds the "active" class to the current step:
        x[n].className += " active";
    }

    function calculateProduct() {
        // console.log("break 3");
        console.log("calcProduct1");
        return new Promise(function(resolve, reject){
        var deviceChecks = document.getElementsByName("meddev");
        var deviceUsed = [];
            for (i = 0; i < deviceChecks.length; i++) {
                // If a field is empty...
                if (deviceChecks[i].checked) {
                    deviceUsed.push(deviceChecks[i].value);
            }}
            
            console.log("Device used:" + deviceUsed);
            // https://api.jsonbin.io/b/5eb23bd48284f36af7b645fc
            let req = new XMLHttpRequest();
            console.log("calcProduct2");
            req.onreadystatechange = () => {
                if (req.readyState == XMLHttpRequest.DONE) {
                    var jsonData = JSON.parse(req.responseText);
                    for(i = 0; i<deviceUsed.length;i++){
                    var prodList = jsonData[deviceUsed[i]];
                    for(var j = 0;j<prodList.length;j++){
                        if(!productRec.includes(prodList[j])){
                        productRec.push(prodList[j]);}
                    }
                    // console.log("Recommended device: "+ productRec);
                    // console.log("All jSON Data: " + req.responseText);
                }}
        };
        console.log("calcProduct3");
        req.open("GET", "https://api.jsonbin.io/b/5eb23bd48284f36af7b645fc/3", true);
        req.send();

        resolve('Calc Prod Done');
        });
    }
    
        function getProductInfo(url, productName){
        var urls = []; 
        return new Promise(function(resolve,reject){
            $.get(proxyurl+url, function(data) { 
            var product_gridTable = gridTable[productName];
            console.log("adding new product");
            product_gridTable[0] = $('h1.product-single__title', data).html();
            var imgs = $('<div class= "product-single__photos" style="user-select: auto;">').html(data).find('img');
                imgs.each(function(i, img) {
                    urls.push(img.src); 
                });
                product_gridTable[1] = urls[3];
            });
            resolve('finished getting product info');
        });
        }

function getBlogInfo(url, productName){
    var urls = []; 
    return new Promist(function(resulve,reject){
        var product_gridTable = gridTable[productName];
        console.log("adding new blog");
        $.get(proxyurl+url, function(data) {
            product_gridTable[2] = $('h1.post-title',data).html();         
            product_gridTable[3]= $('div.post-image',data).find('a').attr('href');
        });
        resolve('finished getting blog info');
    });
}

    //og: https://www.quora.com/How-do-detect-if-an-image-URL-s-isn-t-able-to-load-in-browser-side-Java-script
        function validatingurl (url){
            fetch(proxyurl+url, { method: 'HEAD' })
                .then(res => {
                    if (res.ok) {
                    //	console.log("working url image"); 
                        return true; 
                    } else {
                    //	console.log("not working"); 
                        console.log(url); 
                        return false;
                    }
                }).catch(err => console.log('Error:', err));
        } 
        
        function siftingundefinedurl(url){
            if (url == undefined){
                const backupurl = "https://media-exp1.licdn.com/dms/image/C510BAQHyvU-y9nCIoQ/company-logo_200_200/0?e=2159024400&v=beta&t=sgqg7XGUctmF9h5H5-_OWSJIOfhFnkhwEyBBqgLsITo" ; 
                //console.log("doesnt work");
                return backupurl; 
            }
            else{
                console.log("works"); 
                return url; 
            }
        }
        function siftingundefinedname(product_name, productName){
            if (product_name == undefined){
                //console.log("dont works"); 
                return productName; 
            }
            else{
                console.log("works"); 
                return product_name; 
            }
        }

    function createGrid(){
        console.log(gridTable);
        for(var i =0; i<Object.keys(gridTable).length; i++){
        var productLabel = productRec[i];
        console.log("product label: "+ productLabel);
        const product = Object.getOwnPropertyDescriptor(gridTable, productLabel).value;
        console.log("product from grid table: ");
        console.log(product)
        var productURLList =  CATALOG[productLabel];
        console.log("product urls: ")
        console.log(productURLList);

        var imageURL = product[1];
        var productName = product[0];
        var productURL = productURLList[0];
        console.log("image url: "+imageURL);
        console.log("product name: "+productName);
        console.log("product url: "+productURL);

            var node = document.getElementById("productCardTmp").cloneNode(true);
            node.id = productLabel;
            node.className = "product-card-box";
            console.log("product image url"+product[1]);
            //var imageurl = siftingundefinedurl(product[1]); 
            node.querySelector(".product-image img").setAttribute("src", imageURL);
            //node.querySelector(".product-image img").setAttribute("src", imageurl); //add image
            node.querySelector(".product-image a").setAttribute("href", productURL); //add link to image
            // var product_name = siftingundefinedname(product[0], productName); 
            node.querySelector(".product-info h5").innerText = productName; // add name
            // node.querySelector(".product-info h5").innerText = product_name; // add name
            node.querySelector(".product-info a").setAttribute("href", productURL); //add link to name
            document.querySelector(".productContainer").appendChild(node);} //add new card 
        
    }

    function createBlog(){
        var productName = productRec[0];
        var product = gridTable[productName];
        var productURL = CATALOG[productName];
        console.log("blog url " + product[3]);
        document.querySelector(".blog-image img").setAttribute("src", product[3]);  // add blog image
        document.querySelector(".blog-image a").setAttribute("href", productURL[1]); // add blog url to image
        console.log("blog name " + product[2]);
        document.querySelector(".blog-name h4").innerText = product[2]; // add blog name
        document.querySelector(".blog-name a").setAttribute("href", productURL[1]); // add blog url to name
    }
        
    async function buildGridTable(){
        for(var i=0; i<productRec.length;i++){
            var x = productRec[i];
            gridTable[x] = [];
            let productURLs = CATALOG[productRec[i]];
        var resultFromProductInfo = await getProductInfo(productURLs[0],productRec[i]);
        console.log(resultFromProductInfo);
        var resultFromBlogInfo = await getBlogInfo(productURLs[1], productRec[i]);
        console.log(resultFromBlogInfo);
        }
    }
    function createProductPage(){
        console.log(gridTable);
        createBlog();
        createGrid();
    }

    function displayProductPage(){
        document.querySelector("#container").style.backgroundColor = "white";
        document.querySelector("#container").style.paddingTop = "50px";
        document.querySelector("#title").remove();
        document.querySelector("#description").remove();
    }