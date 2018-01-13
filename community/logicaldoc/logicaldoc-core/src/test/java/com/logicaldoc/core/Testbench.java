package com.logicaldoc.core;

import com.logicaldoc.core.automation.SystemTool;
import com.logicaldoc.core.communication.EMailSender;

public class Testbench {

	public static void main(String[] args) throws Exception {
		// Automation engine = new Automation(null, null, 1L);

		SystemTool tool = new SystemTool();
		System.out.println("out> "+tool.execGetOutput("D:\\tmp\\hello.bat"));
		

		// String original="1234567€";
		// System.out.println("original: "+original);
		//
		// String encoded=URLEncoder.encode(original, "UTF-8");
		// System.out.println("encoded: "+encoded);
		//
		//
		// String decoded=URLDecoder.decode("1234567%E2%82%AC", "UTF-8");
		// System.out.println("decoded: "+decoded);

		// EMailSender sender = prepareEmailSender();
		//
		// EMail eml = new EMail();
		// eml.setSubject("HTML test");
		// eml.setHtml(1);
		// Set<Recipient> recipients = new HashSet<Recipient>();
		//
		// eml.getImages().add("file:///C:/Users/Marco/Pictures/email_header2.jpg");
		// //"http://www.logicaldoc.com/templates/theme326/images/LogicalDOC_logo_website.gif");
		//
		// // Notify the general report
		// Recipient rec = new Recipient();
		// rec.setAddress("a.gasparini@logicalobjects.com");
		// recipients.add(rec);
		// eml.setRecipients(recipients);
		// //
		// eml.setMessageText("<span style='color:red'>Test</span> di <b>HTML</b>\ncippa"
		// // +"<br/><image src='cid:image_1' />");
		//
		// String message = StringUtil.writeToString(new FileInputStream(new
		// File("C:/tmp/ldoc_ready.html")), "UTF8");
		// message=message.replaceAll("\\{", "'{'");
		// message=message.replaceAll("}", "'}'");
		// message=message.replaceAll("\\[\\[", "{");
		// message=message.replaceAll("\\]\\]", "}");
		//
		//
		// MessageFormat format = new MessageFormat(message);
		// message = format.format(new Object[] { "Marco Meschieri",
		// "qui c'è il codice di attivazione" });
		// eml.setMessageText(message);
		//
		// System.out.println(eml.getMessageText());
		//
		// sender.send(eml);
	}

	protected static EMailSender prepareEmailSender() {
		EMailSender sender = new EMailSender();
		sender.setHost("smtp.logicalobjects.com");
		sender.setPort(25);
		sender.setUsername("m.meschieri@logicalobjects.com");
		sender.setPassword("paf75peR");
		sender.setAuthEncripted(false);
		sender.setConnectionSecurity(0);
		sender.setSender("m.meschieri@logicalobjects.com");
		return sender;
	}
}
