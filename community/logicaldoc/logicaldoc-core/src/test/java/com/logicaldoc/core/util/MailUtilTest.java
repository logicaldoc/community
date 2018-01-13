package com.logicaldoc.core.util;

import java.io.File;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.Recipient;

/**
 * Test case for the <code>MailUtil</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class MailUtilTest extends AbstractCoreTCase {

	@Test
	public void testMsgToMail() throws Exception {
		{
			EMail mail = MailUtil.msgToMail(new File("src/test/resources/test.msg"), true);
			Assert.assertNotNull(mail);
			Assert.assertEquals("Re: Fwd: Offer for Customizations on LogicalDOC",
					mail.getSubject());
			Assert.assertTrue(mail.getMessageText().contains("Stefan"));
			Assert.assertEquals(1, mail.getAttachmentsCount());
			Assert.assertEquals("erich.widmer@olig.ch", mail.getFrom().getAddress());
		}
	}
	
	@Test
	public void testMessageToMail() throws Exception {
		{
			EMail mail = MailUtil.messageToMail(new File("src/test/resources/amazon.eml"), true);
			Assert.assertNotNull(mail);
			Assert.assertEquals("Il tuo ordine Amazon.it (#403-7782228-1569116) )",
					mail.getSubject());
			Assert.assertTrue(mail.getMessageText().startsWith("<html"));
			Assert.assertEquals(0, mail.getAttachmentsCount());
			Assert.assertEquals("delivery-notification@amazon.it", mail.getFrom().getAddress());
		}
		
		{
			EMail mail = MailUtil.messageToMail(new File("src/test/resources/kajima.eml"), true);
			Assert.assertNotNull(mail);
			Assert.assertEquals("Project Kajima - RFI",
					mail.getSubject());
			Assert.assertTrue(mail.getMessageText().startsWith("<html"));
			Assert.assertTrue(mail.getMessageText().contains("Colin"));
			Assert.assertEquals(3, mail.getAttachmentsCount());
			Assert.assertEquals("Introduction Letter and Instructions.pdf", mail.getAttachment(1)
					.getFileName());
			Assert.assertEquals(71663, mail.getAttachment(1).getData().length);
			Set<Recipient> to = mail.getRecipients();
			Assert.assertEquals(1, to.size());
			Assert.assertEquals("m.meschieri@logicaldoc.com", to.iterator().next().getName());
			Assert.assertEquals("m.meschieri@logicaldoc.com", to.iterator().next().getAddress());
			Assert.assertEquals("vendor_info@kajima.co.uk", mail.getReplyTo().toString());
		}
		
		{
			EMail mail = MailUtil.messageToMail(new File("src/test/resources/parche 2.eml"), true);
			Assert.assertNotNull(mail);
			Assert.assertEquals("RE: parche 2", mail.getSubject());
			Assert.assertEquals(0, mail.getAttachmentsCount());
			Set<Recipient> to = mail.getRecipients();
			Assert.assertEquals(1, to.size());
			Assert.assertEquals("'Marco Meschieri'", to.iterator().next().getName());
			Assert.assertEquals("m.meschieri@logicaldoc.com", to.iterator().next().getAddress());
			Assert.assertEquals("xcumplido@ingenium-ax.com.mx", mail.getReplyTo().toString());
			Assert.assertTrue(mail.getRecipientsCC().isEmpty());
			Assert.assertEquals("Xavier Cumplido Morales", mail.getAuthor());
			Assert.assertEquals("xcumplido@ingenium-ax.com.mx", mail.getAuthorAddress());
			Assert.assertTrue(mail.getMessageText().startsWith("<html"));
			Assert.assertTrue(mail.getMessageText().contains("Saludos"));
		}
	}
}
