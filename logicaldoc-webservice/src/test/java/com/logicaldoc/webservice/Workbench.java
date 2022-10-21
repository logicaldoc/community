package com.logicaldoc.webservice;

public class Workbench {

	public static void main(String[] args) {
		String sid=null;
//		String content = "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"><soap:Body><ns2:logout xmlns:ns2=\"http://ws.logicaldoc.com\"><sid xmlns=\"\">9f0fb827-78ee-4cce-998e-20672c03271f</sid></ns2:logout></soap:Body></soap:Envelope>";
//		String pattern = "<sid(.*)>([\\w\\-\\d]*)</sid>";

//		String content = "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"><soap:Body><ns2:login xmlns:ns2=\"http://ws.logicaldoc.com\"><username>admin</username><password>12345678</password></ns2:login></soap:Body></soap:Envelope>";
//		String pattern = "<username(.*)>([\\w\\-\\d]*)</username>";
		
//		String content = "http://www.example.com?password=polèkuju(lo&att1=billo&password=pluto";
		String content = "http://localhost:9080/services/rest/auth/login";
		
		System.out.println(WebserviceInterceptor.maskCredentials(content));
		
		
//		String pattern = "password=[^&.]*";
//		
//		Pattern regPat = Pattern.compile(pattern);
//		Matcher matcher = regPat.matcher(content);
//		while (matcher.find()) {
//			String matchedText = matcher.group();
//			
//			System.out.println("matched: "+matchedText);
//			content=content.replace(matchedText, "password=XXXX");
////			sid = matchedText.substring(matchedText.indexOf('>')+1,  matchedText.lastIndexOf('<'));
//		}
//		System.out.println(content);
		
//		String maskedSid = StringUtils.overlay(sid, StringUtils.repeat("X", sid.length()-6), 0, sid.length()-6);
//		String maskedContent = content.replace(sid, maskedSid);
//		
//		System.out.println("sid: "+sid);
//		System.out.println("maskedSid: "+maskedSid);
//		System.out.println("maskedContent: "+maskedContent);

	}

}
