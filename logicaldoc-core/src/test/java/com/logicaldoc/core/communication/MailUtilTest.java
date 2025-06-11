package com.logicaldoc.core.communication;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Set;

import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;

import org.bouncycastle.cms.CMSException;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;

/**
 * Test case for the <code>MailUtil</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class MailUtilTest extends AbstractCoreTestCase {

	@Test
	public void testMsgToMail() throws IOException, MessagingException, CMSException  {
		EMail mail = MailUtil.msgToMail(new File("src/test/resources/test.msg"), true);

		assertNotNull(mail);
		assertEquals("Re: Fwd: Offer for Customizations on LogicalDOC", mail.getSubject());
		assertTrue(mail.getMessageText().contains("Stefan"));
		assertEquals(1, mail.getAttachmentsCount());
		assertEquals("erich.widmer@olig.ch", mail.getFrom().getAddress());

		assertNotNull(mail.getSentDate());
		assertNotNull(mail.getReceivedDate());
		assertEquals(mail.getSentDate(), mail.getReceivedDate());
	}

	@Test
	public void testSignedMsg() throws IOException, MessagingException, CMSException  {
		EMail mail = MailUtil.msgToMail(new File("src/test/resources/signed.msg"), true);
		assertEquals("Invio messaggio SMIME (signed and clear text)", mail.getSubject());
		assertEquals("3 crucial benefits of Cloud computing.docx", mail.getAttachments().get(1).getFileName());
	}

	@Test
	public void testContainsAttachments()  {
		assertTrue(MailUtil.msgContainsAttachments(new File("src/test/resources/signed.msg")));
		assertTrue(MailUtil.emlContainsAttachments(new File("src/test/resources/fattura.eml")));
	}

	@Test
	public void testReadMime() throws MessagingException, IOException  {
		EMail mail = MailUtil.messageToMail(new File("src/test/resources/fattura.eml"), false);
		assertNotNull(mail);
	}

	@Test
	public void testMessageToMail() throws MessagingException, IOException  {
		{
			EMail mail = MailUtil.messageToMail(new File("src/test/resources/abel.eml"), true);
			assertNotNull(mail);
			assertEquals(1, mail.getAttachmentsCount());
		}

		{
			EMail mail = MailUtil.messageToMail(new File("src/test/resources/amazon.eml"), true);
			assertNotNull(mail);
			assertEquals("Il tuo ordine Amazon.it (#403-7782228-1569116) )", mail.getSubject());
			assertTrue(mail.getMessageText().startsWith("<html"));

			assertEquals(0, mail.getAttachmentsCount());

			assertEquals("delivery-notification@amazon.it", mail.getFrom().getAddress());
		}

		{
			EMail mail = MailUtil.messageToMail(new File("src/test/resources/kajima.eml"), true);
			assertNotNull(mail);
			assertEquals("Project Kajima - RFI", mail.getSubject());
			assertTrue(mail.getMessageText().startsWith("<html"));
			assertTrue(mail.getMessageText().contains("Colin"));
			assertEquals(3, mail.getAttachmentsCount());
			assertEquals("Introduction Letter and Instructions.pdf", mail.getAttachment(1).getFileName());
			assertEquals(71663, mail.getAttachment(1).getData().length);
			Set<Recipient> to = mail.getRecipients();
			assertEquals(1, to.size());
			assertEquals("m.meschieri@logicaldoc.com", to.iterator().next().getName());
			assertEquals("m.meschieri@logicaldoc.com", to.iterator().next().getAddress());

			assertTrue(mail.getReplyTo().toString().contains("vendor_info@kajima.co.uk"));
		}
	}

	@Test
	public void testMessageToMailWithAttachments() throws MessagingException, IOException  {
		EMail mail = MailUtil.messageToMail(new File("src/test/resources/New email with attachments.eml"), true);
		assertNotNull(mail);
		assertEquals(1, mail.getAttachmentsCount());
		
		mail = MailUtil.messageToMail(new File("src/test/resources/test with attachment.eml"), true);
		assertNotNull(mail);
		assertEquals(6, mail.getAttachmentsCount());		
	}

	@Test
	public void testMessageToMailB() throws MessagingException, IOException  {
		EMail mail = MailUtil.messageToMail(new File("src/test/resources/parche 2.eml"), true);
		assertNotNull(mail);
		assertEquals("RE: parche 2", mail.getSubject());
		assertEquals(10, mail.getAttachmentsCount());
		Set<Recipient> to = mail.getRecipients();
		assertEquals(1, to.size());
		assertEquals("'Marco Meschieri'", to.iterator().next().getName());
		assertEquals("m.meschieri@logicaldoc.com", to.iterator().next().getAddress());

		assertTrue(mail.getReplyTo().toString().contains("xcumplido@ingenium-ax.com.mx"));

		assertTrue(mail.getRecipientsCC().isEmpty());
		assertEquals("Xavier Cumplido Morales", mail.getAuthor());
		assertEquals("xcumplido@ingenium-ax.com.mx", mail.getAuthorAddress());
		assertTrue(mail.getMessageText().startsWith("<html"));
		assertTrue(mail.getMessageText().contains("Saludos"));

		// An email with another email inside
		mail = MailUtil.messageToMail(new File("src/test/resources/email2022-00398.eml"), true);
		assertNotNull(mail);
		assertEquals(1, mail.getAttachmentsCount());
		EMailAttachment attachment = mail.getAttachment(1);
		assertEquals("I: test mail 2.eml", attachment.getFileName());

		mail = MailUtil.messageToMail(new File("src/test/resources/MAHAG_-_Ihre_elektronische_Rechnung.eml"), true);
		assertNotNull(mail);
		assertEquals(1, mail.getAttachmentsCount());
	}

	@Test
	public void testExtractMessageText() throws MessagingException, IOException {
		MimeMessage mail = MailUtil.readMime(this.getClass().getResourceAsStream("/parche 2.eml"));
		assertNotNull(mail);

		assertTrue(MailUtil.extractMessageText(mail).startsWith("Hola Marco, el parche 2"));
	}

	@Test
	public void testSMIME() throws MessagingException, IOException {
		EMail mail = MailUtil.messageToMail(new File("src/test/resources/ArchivBelege_vom_12_05_2023.eml"), true);
		assertNotNull(mail);
		assertEquals(6, mail.getAttachmentsCount());
	}
}
