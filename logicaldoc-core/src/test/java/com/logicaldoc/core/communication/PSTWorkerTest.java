package com.logicaldoc.core.communication;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

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
	public void testPrintListing() throws IOException {
		PSTWorker worker = new PSTWorker(new File("src/test/resources/sample.pst"));
		String listing = worker.printListing();
		assertNotNull(listing);
		assertTrue(listing.contains("New message created by"));
	}

	@Test
	public void testCountEmails() throws IOException {
		PSTWorker worker = new PSTWorker(new File("src/test/resources/sample.pst"));
		long count = worker.countEmails();
		assertEquals(1, count);
	}

	@Test
	public void testPrintEmails() throws IOException {
		PSTWorker worker = new PSTWorker(new File("src/test/resources/sample.pst"));
		String emails = worker.printEmails();
		assertNotNull(emails);
		assertTrue(emails.contains("This line is in bold"));
	}
}