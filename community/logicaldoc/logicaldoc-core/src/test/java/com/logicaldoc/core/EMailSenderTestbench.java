package com.logicaldoc.core;

import java.util.Date;

import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.security.Tenant;

public class EMailSenderTestbench {

	public static void main(String[] args) throws Exception {
		EMailSender sender = new EMailSender(Tenant.DEFAULT_NAME);
		
		sender.setHost("smtp.office365.com");
		sender.setPassword("");
		sender.setUsername("");
		sender.setSender("");
		sender.setAuthEncripted(false);
		sender.setConnectionSecurity(EMailSender.SECURITY_TLS);
		sender.setPort(587);
		
		EMail mail;
		mail = new EMail();
		mail.setAccountId(-1);
		mail.setAuthor("");
		mail.setAuthorAddress("");
		mail.parseRecipients("");
		mail.setFolder("outbox");
		mail.setSentDate(new Date());
		mail.setSubject("test");
		mail.setMessageText("test");
		
		sender.send(mail);
	}

}
