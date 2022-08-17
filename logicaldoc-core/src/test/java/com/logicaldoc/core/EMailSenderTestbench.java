package com.logicaldoc.core;

import java.util.Date;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;

public class EMailSenderTestbench {

	public static void main(String[] args) throws Exception {
		EMail mail;
		mail = new EMail();
		mail.setAccountId(-1);
		mail.setAuthor("marco");
		mail.setAuthorAddress("m.meschieri@logicaldoc.com");
		mail.parseRecipients("m.meschieri@logicaldoc.com");
		mail.setFolder("outbox");
		mail.setSentDate(new Date());
		mail.setSubject("test");
		mail.setMessageText("test");

// OFFICE 365 (TLS v1.2)
//		EMailSender sender = new EMailSender();
//		sender.setHost("smtp.office365.com");
//		sender.setPassword("");
//		sender.setUsername("");
//		sender.setSender("");
//		sender.setAuthEncrypted(false);
//		sender.setConnectionSecurity(EMailSender.SECURITY_TLS);
//		sender.setPort(587);
//		sender.send(mail);
		
//		EMailSender sender = new EMailSender();
//		sender.setHost("smtp.office365.com");
//		sender.setPassword("a@_^JqZX5bI]nA/");
//		sender.setUsername("m.meschieri@logicaldoc.com");
//		sender.setSender("m.meschieri@logicaldoc.com");
//		sender.setAuthEncrypted(false);
//		sender.setConnectionSecurity(EMailSender.SECURITY_TLS);
//		sender.setPort(587);
//		sender.send(mail);

// ARUBA SMTP		
//		EMailSender sender = new EMailSender();
//		sender.setHost("smtp.logicaldoc.com");
//		sender.setPassword("paf75peRQT");
//		sender.setUsername("m.meschieri@logicaldoc.com");
//		sender.setSender("m.meschieri@logicaldoc.com");
//		sender.setAuthEncrypted(false);
//		sender.setConnectionSecurity(EMailSender.SECURITY_NONE);
//		sender.setPort(25);
//		sender.send(mail);

// GMAIL SSL  (requires to define an App password in Google security)
//		EMailSender sender = new EMailSender();
//		sender.setHost("smtp.gmail.com");
//		sender.setPassword("****");
//		sender.setUsername("marco.meschieri@gmail.com");
//		sender.setSender("marco.meschieri@gmail.com");
//		sender.setAuthEncrypted(false);
//		sender.setConnectionSecurity(EMailSender.SECURITY_SSL);
//		sender.setPort(465);
//		sender.send(mail);

// GMAIL TLS (TLS v1.2)
//		EMailSender sender = new EMailSender();
//		sender.setHost("smtp.gmail.com");
//		sender.setPassword("****");
//		sender.setUsername("marco.meschieri@gmail.com");
//		sender.setSender("marco.meschieri@gmail.com");
//		sender.setAuthEncrypted(false);
//		sender.setConnectionSecurity(EMailSender.SECURITY_TLS);
//		sender.setPort(587);
//		sender.send(mail);

		
		System.out.println("Sent " + mail.getSubject());
	}

}
