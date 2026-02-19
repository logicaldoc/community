package com.logicaldoc.webserviceclient;

import java.io.File;
import java.io.IOException;

import org.xmlpull.v1.XmlPullParserException;

public class WebserviceClientTestbench {

	public static void main(String[] args) throws IOException, XmlPullParserException {
		WebserviceClient client =  WebserviceClient.get("49112bd1-cd79-4643-91a4-a5cbe326da48","http://localhost:9080");
		client.uploadFile(4L, new File("C:\\Users\\marco\\Downloads\\sample.jpg"), "sample.jpg", "en");
	}
}