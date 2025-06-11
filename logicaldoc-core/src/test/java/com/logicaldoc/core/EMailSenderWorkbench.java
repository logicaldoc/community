package com.logicaldoc.core;

import java.util.Date;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;

public class EMailSenderWorkbench {

	public static void main(String[] args) throws Exception {
		EMail mail;
		mail = new EMail();
		mail.setAccountId(-1);
		mail.setAuthor("marco");
		mail.setAuthorAddress("adminex@logicaldoc.365.cloud");
		mail.parseRecipients("m.meschieri@logicaldoc.com");
		mail.setFolder("outbox");
		mail.setSentDate(new Date());
		mail.setSubject("test");
		mail.setMessageText("test");

		EMailSender sender = new EMailSender();
		
// OFFICE 365
//		sender.setHost("smtp.office365.com");
//		sender.setPassword("****");
//		sender.setPort(587);
//		sender.setProtocol("smtpmicrosoft365");
//		sender.setConnectionSecurity(EMailSender.SECURITY_STARTTLS);
//		sender.setAuthEncrypted(true);
//		sender.setUsername("adminex@logicaldoc.365.cloud");
//		sender.setSender("adminex@logicaldoc.365.cloud");


		
		
// ARUBA SMTP		
//		sender.setHost("smtp.logicaldoc.com");
//		sender.setPassword("****");
//		sender.setUsername("m.meschieri@logicaldoc.com");
//		sender.setSender("m.meschieri@logicaldoc.com");
//		sender.setAuthEncrypted(false);
//		sender.setConnectionSecurity(EMailSender.SECURITY_NONE);
//		sender.setPort(25);

// GMAIL SSL  (requires to define an App password in Google security)
//		sender.setHost("smtp.gmail.com");
//		sender.setPassword("+++put application password+++");
//		sender.setUsername("marco.meschieri@gmail.com");
//		sender.setSender("marco.meschieri@gmail.com");
//		sender.setAuthEncrypted(false);
//		sender.setConnectionSecurity(EMailSender.SECURITY_SSL);
//		sender.setPort(465);

// GMAIL TLS (TLS v1.2)
//		sender.setHost("smtp.gmail.com");
//		sender.setPassword("****");
//		sender.setUsername("marco.meschieri@gmail.com");
//		sender.setSender("marco.meschieri@gmail.com");
//		sender.setAuthEncrypted(false);
//		sender.setConnectionSecurity(EMailSender.SECURITY_TLS);
//		sender.setPort(587);

		sender.send(mail);
		
		System.out.println("Sent " + mail.getSubject());
	}

}
