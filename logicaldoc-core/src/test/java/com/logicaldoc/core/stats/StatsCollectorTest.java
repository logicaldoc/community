package com.logicaldoc.core.stats;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.apache.hc.core5.http.NameValuePair;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.util.plugin.PluginException;

public class StatsCollectorTest extends AbstractCoreTestCase {

	// Instance under test
	private StatsCollector testSubject = new StatsCollector();

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		testSubject = (StatsCollector) context.getBean("StatsCollector");
		testSubject.setUploadStatistics(false);
	}

	@Test
	public void testRun() {
		testSubject.run();

		List<NameValuePair> stats = testSubject.getStatistics();
		assertEquals("7", stats.stream().filter(p -> "docs".equals(p.getName())).findFirst().orElse(null).getValue());
		assertEquals("11",
				stats.stream().filter(p -> "folders".equals(p.getName())).findFirst().orElse(null).getValue());
	}
}