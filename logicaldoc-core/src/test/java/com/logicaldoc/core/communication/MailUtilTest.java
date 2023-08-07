package com.logicaldoc.core.communication;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.Set;
import java.util.stream.Collectors;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;

import junit.framework.Assert;

/**
 * Test case for the <code>MailUtil</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class MailUtilTest extends AbstractCoreTestCase {

	@Test
	public void testMsgToMail() throws Exception {
		EMail mail = MailUtil.msgToMail(new File("src/test/resources/test.msg"), true);

		Assert.assertNotNull(mail);
		Assert.assertEquals("Re: Fwd: Offer for Customizations on LogicalDOC", mail.getSubject());
		Assert.assertTrue(mail.getMessageText().contains("Stefan"));
		Assert.assertEquals(1, mail.getAttachmentsCount());
		Assert.assertEquals("erich.widmer@olig.ch", mail.getFrom().getAddress());

		Assert.assertNotNull(mail.getSentDate());
		Assert.assertNotNull(mail.getReceivedDate());
		assertEquals(mail.getSentDate(), mail.getReceivedDate());
	}

	@Test
	public void testSignedMsg() throws Exception {
		EMail mail = MailUtil.msgToMail(new File("src/test/resources/signed.msg"), true);
		Assert.assertEquals("Invio messaggio SMIME (signed and clear text)", mail.getSubject());
		Assert.assertEquals("3 crucial benefits of Cloud computing.docx", mail.getAttachments().get(1).getFileName());
	}

	@Test
	public void testContainsAttachments() throws Exception {
		Assert.assertTrue(MailUtil.msgContainsAttachments(new File("src/test/resources/signed.msg")));
		Assert.assertTrue(MailUtil.emlContainsAttachments(new File("src/test/resources/fattura.eml")));
	}

	@Test
	public void testReadMime() throws Exception {
		EMail mail = MailUtil.messageToMail(new File("src/test/resources/fattura.eml"), false);
		Assert.assertNotNull(mail);
	}

	@Test
	public void testMessageToMail() throws Exception {
		{
			EMail mail = MailUtil.messageToMail(new File("src/test/resources/abel.eml"), true);
			Assert.assertNotNull(mail);
			Assert.assertEquals(1, mail.getAttachmentsCount());
		}

		{
			EMail mail = MailUtil.messageToMail(new File("src/test/resources/amazon.eml"), true);
			Assert.assertNotNull(mail);
			Assert.assertEquals("Il tuo ordine Amazon.it (#403-7782228-1569116) )", mail.getSubject());
			Assert.assertTrue(mail.getMessageText().startsWith("<html"));

			Assert.assertEquals(0, mail.getAttachmentsCount());

			Assert.assertEquals("delivery-notification@amazon.it", mail.getFrom().getAddress());
		}

		{
			EMail mail = MailUtil.messageToMail(new File("src/test/resources/kajima.eml"), true);
			Assert.assertNotNull(mail);
			Assert.assertEquals("Project Kajima - RFI", mail.getSubject());
			Assert.assertTrue(mail.getMessageText().startsWith("<html"));
			Assert.assertTrue(mail.getMessageText().contains("Colin"));
			Assert.assertEquals(3, mail.getAttachmentsCount());
			Assert.assertEquals("Introduction Letter and Instructions.pdf", mail.getAttachment(1).getFileName());
			Assert.assertEquals(71663, mail.getAttachment(1).getData().length);
			Set<Recipient> to = mail.getRecipients();
			Assert.assertEquals(1, to.size());
			Assert.assertEquals("m.meschieri@logicaldoc.com", to.iterator().next().getName());
			Assert.assertEquals("m.meschieri@logicaldoc.com", to.iterator().next().getAddress());

			Assert.assertTrue(mail.getReplyTo().toString().contains("vendor_info@kajima.co.uk"));
		}
	}

	@Test
	public void testMessageToMailWithAttachments() throws Exception {
		EMail mail = MailUtil.messageToMail(new File("src/test/resources/New email with attachments.eml"), true);
		Assert.assertNotNull(mail);
		Assert.assertEquals(1, mail.getAttachmentsCount());
	}

	@Test
	public void testMessageToMailB() throws Exception {
		EMail mail = MailUtil.messageToMail(new File("src/test/resources/parche 2.eml"), true);
		Assert.assertNotNull(mail);
		Assert.assertEquals("RE: parche 2", mail.getSubject());
		Assert.assertEquals(10, mail.getAttachmentsCount());
		Set<Recipient> to = mail.getRecipients();
		Assert.assertEquals(1, to.size());
		Assert.assertEquals("'Marco Meschieri'", to.iterator().next().getName());
		Assert.assertEquals("m.meschieri@logicaldoc.com", to.iterator().next().getAddress());

		Assert.assertTrue(mail.getReplyTo().toString().contains("xcumplido@ingenium-ax.com.mx"));

		Assert.assertTrue(mail.getRecipientsCC().isEmpty());
		Assert.assertEquals("Xavier Cumplido Morales", mail.getAuthor());
		Assert.assertEquals("xcumplido@ingenium-ax.com.mx", mail.getAuthorAddress());
		Assert.assertTrue(mail.getMessageText().startsWith("<html"));
		Assert.assertTrue(mail.getMessageText().contains("Saludos"));

		// An email with another email inside
		mail = MailUtil.messageToMail(new File("src/test/resources/email2022-00398.eml"), true);
		Assert.assertNotNull(mail);
		Assert.assertEquals(1, mail.getAttachmentsCount());
		EMailAttachment attachment = mail.getAttachment(1);
		assertEquals("I: test mail 2.eml", attachment.getFileName());

		mail = MailUtil.messageToMail(new File("src/test/resources/MAHAG_-_Ihre_elektronische_Rechnung.eml"), true);
		Assert.assertNotNull(mail);
		Assert.assertEquals(1, mail.getAttachmentsCount());
	}

	@Test
	public void testExtractMessageText() throws MessagingException, IOException {
		MimeMessage mail = MailUtil.readMime(this.getClass().getResourceAsStream("/parche 2.eml"));
		Assert.assertNotNull(mail);

		Assert.assertTrue(MailUtil.extractMessageText(mail).startsWith("Hola Marco, el parche 2"));
	}

	@Test
	public void testSMIME() throws MessagingException, IOException {
		EMail mail = MailUtil.messageToMail(new File("src/test/resources/ArchivBelege_vom_12_05_2023.eml"), true);
		Assert.assertNotNull(mail);
		Assert.assertEquals(6, mail.getAttachmentsCount());
	}
}
