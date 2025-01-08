package com.logicaldoc.webserviceclient;

import java.io.IOException;

import org.xmlpull.v1.XmlPullParserException;

public class WebserviceClientTestbench {


	public static void main(String[] args) throws IOException, XmlPullParserException {
		WebserviceClient client =  WebserviceClient.get("8e06c2f7-2359-4a36-a1f2-83d8143f7006","http://localhost:9080");
		System.out.println(client.getPath(717513653L));
	}
}