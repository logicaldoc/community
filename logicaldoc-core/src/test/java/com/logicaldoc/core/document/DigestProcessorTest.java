package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Locale;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.task.TaskException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>DigestProcess</code>
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.1.1
 */
public class DigestProcessorTest extends AbstractCoreTestCase {

	private DigestProcessor testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = Context.get(DigestProcessor.class);
	}

	@Test
	public void testRunTask() throws TaskException {
		testSubject.runTask();
		
		Context.get().getProperties().setProperty("digest.batch", "10");
		assertEquals("10", Context.get().getProperties().getProperty("digest.batch"));
		
		Context.get().getProperties().setProperty("digest.batch", "2");
		assertEquals("2", Context.get().getProperties().getProperty("digest.batch"));
		
		Context.get().getProperties().setProperty("digest.batch", "1000");
		assertEquals("1000", Context.get().getProperties().getProperty("digest.batch"));
		
		assertFalse(testSubject.isIndeterminate());
		assertTrue(testSubject.isConcurrent());
	}

	@Test
	public void testPrepareReport() {
		assertNotNull(testSubject.prepareReport(Locale.ENGLISH));
	}
}
