package com.logicaldoc.core.communication;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;

/**
 * Test case for the <code>PSTWorker</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class PSTWorkerTest extends AbstractCoreTestCase {

	@Test
	public void testPrintListing() throws FileNotFoundException, IOException {
		PSTWorker worker = new PSTWorker(new File("src/test/resources/sample.pst"));
		String listing = worker.printListing();
		Assert.assertNotNull(listing);
		Assert.assertTrue(listing.contains("New message created by"));
	}

	@Test
	public void testCountEmails() throws FileNotFoundException, IOException {
		PSTWorker worker = new PSTWorker(new File("src/test/resources/sample.pst"));
		long count = worker.countEmails();
		Assert.assertEquals(1, count);
	}
	
	@Test
	public void testPrintEmails() throws FileNotFoundException, IOException {
		PSTWorker worker = new PSTWorker(new File("src/test/resources/sample.pst"));
		String emails = worker.printEmails();
		Assert.assertNotNull(emails);
		Assert.assertTrue(emails.contains("This line is in bold"));
		System.out.println(emails);
	}
}