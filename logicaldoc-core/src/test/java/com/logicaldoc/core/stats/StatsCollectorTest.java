package com.logicaldoc.core.stats;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.apache.hc.core5.http.NameValuePair;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

public class StatsCollectorTest extends AbstractCoreTestCase {

	// Instance under test
	private StatsCollector testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		testSubject = Context.get(StatsCollector.class);
		testSubject.setUploadStatistics(false);
	}

	@Test
	public void testRun() {
		testSubject.run();

		List<NameValuePair> stats = testSubject.getStatistics();

		assertEquals("9", stats.stream().filter(p -> "docs".equals(p.getName())).findFirst().orElse(null).getValue());
		assertEquals("18",
				stats.stream().filter(p -> "folders".equals(p.getName())).findFirst().orElse(null).getValue());
	}

}