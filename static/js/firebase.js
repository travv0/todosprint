// Initialize Firebase
var config = {
    apiKey: "AIzaSyDUEFseQ3-Z9HsFKYfUpTrhkJM7mlFyBFw",
    authDomain: "scheduler-219318.firebaseapp.com",
    databaseURL: "https://scheduler-219318.firebaseio.com",
    projectId: "scheduler-219318",
    storageBucket: "scheduler-219318.appspot.com",
    messagingSenderId: "1003459615329"
};
firebase.initializeApp(config);

const messaging = firebase.messaging();
messaging.usePublicVapidKey('BDLA-h2wfNOktZcaIwU-2Ra07Lb1zcqtNSxBsXBA5wvFWDLeZQdqepVy997ipiNAACYWF4JFvE9F6e43zbVfRSQ');

const tokenDivId = 'token_div';
const permissionDivId = 'permission_div';

messaging.onTokenRefresh(function() {
    messaging.getToken().then(function(refreshedToken) {
        console.log('Token refreshed.');
        setTokenSentToServer(false);
        sendTokenToServer(refreshedToken);
        resetUI();
    }).catch(function(err) {
        console.log('Unable to retrieve refreshed token ', err);
        showToken('Unable to retrieve refreshed token ', err);
    });
});

messaging.onMessage(function(payload) {
    console.log('Message received. ', payload);
    appendMessage(payload);
});

function resetUI() {
    clearMessages();
    showToken('loading...');

    messaging.getToken().then(function(currentToken) {
        if (currentToken) {
            sendTokenToServer(currentToken);
            updateUIForPushEnabled(currentToken);
        } else {
            console.log('No Instance ID token available. Request permission to generate one.');
            updateUIForPushPermissionRequired();
            setTokenSentToServer(false);
        }
    }).catch(function(err) {
        console.log('An error occurred while retrieving token. ', err);
        showToken('Error retrieving Instance ID token. ', err);
        setTokenSentToServer(false);
    });
}
function showToken(currentToken) {
    var tokenElement = document.querySelector('#token');
    tokenElement.textContent = currentToken;
}

function sendTokenToServer(currentToken) {
    if (!isTokenSentToServer()) {
        console.log('Sending token to server...');
        // TODO(developer): Send the current token to your server.
        setTokenSentToServer(true);
    } else {
        console.log('Token already sent to server so won\'t send it again ' +
                    'unless it changes');
    }
}

function isTokenSentToServer() {
    return window.localStorage.getItem('sentToServer') === '1';
}

function setTokenSentToServer(sent) {
    window.localStorage.setItem('sentToServer', sent ? '1' : '0');
}

function showHideDiv(divId, show) {
    const div = document.querySelector('#' + divId);
    if (show) {
        div.style = 'display: visible';
    } else {
        div.style = 'display: none';
    }
}

function requestPermission() {
    console.log('Requesting permission...');
    messaging.requestPermission().then(function() {
        console.log('Notification permission granted.');
        // TODO(developer): Retrieve an Instance ID token for use with FCM.
        resetUI();
    }).catch(function(err) {
        console.log('Unable to get permission to notify.', err);
    });
}
function deleteToken() {
    messaging.getToken().then(function(currentToken) {
        messaging.deleteToken(currentToken).then(function() {
            console.log('Token deleted.');
            setTokenSentToServer(false);
            resetUI();
        }).catch(function(err) {
            console.log('Unable to delete token. ', err);
        });
    }).catch(function(err) {
        console.log('Error retrieving Instance ID token. ', err);
        showToken('Error retrieving Instance ID token. ', err);
    });
}

function appendMessage(payload) {
    const messagesElement = document.querySelector('#messages');
    const dataHeaderELement = document.createElement('h5');
    const dataElement = document.createElement('pre');
    dataElement.style = 'overflow-x:hidden;';
    dataHeaderELement.textContent = 'Received message:';
    dataElement.textContent = JSON.stringify(payload, null, 2);
    messagesElement.appendChild(dataHeaderELement);
    messagesElement.appendChild(dataElement);
}

function clearMessages() {
    const messagesElement = document.querySelector('#messages');
    while (messagesElement.hasChildNodes()) {
        messagesElement.removeChild(messagesElement.lastChild);
    }
}

function updateUIForPushEnabled(currentToken) {
    showHideDiv(tokenDivId, true);
    showHideDiv(permissionDivId, false);
    showToken(currentToken);
}

function updateUIForPushPermissionRequired() {
    showHideDiv(tokenDivId, false);
    showHideDiv(permissionDivId, true);
}

resetUI();
