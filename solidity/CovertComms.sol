pragma solidity ^0.4.0;

contract CovertComms {
    
    event BroadcastInfo(bytes contactCard);
    event Message(address indexed recipient, bytes message);

    function registerUser(bytes contactCard) public {
       BroadcastInfo(contactCard);
    }
		
    function sendMessage(address recipient, bytes message) public {
        Message(recipient, message);
    }
   
}