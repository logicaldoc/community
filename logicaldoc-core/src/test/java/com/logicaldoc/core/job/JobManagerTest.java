package com.logicaldoc.core.job;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.SchedulerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.automation.AutomationDateTool;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link JobManager}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class JobManagerTest extends AbstractCoreTestCase {

	private static final Logger log = LoggerFactory.getLogger(JobManagerTest.class);

	private JobManager testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = Context.get(JobManager.class);
	}

	@Test
	public void testSchedule() throws SchedulerException {
		testSubject.schedule(new AbstractJob("test", "Test", Tenant.DEFAULT_ID) {

			@Override
			public void execute(JobExecutionContext context) throws JobExecutionException {
				log.info("key1 = {}", context.getJobDetail().getJobDataMap().getString("key1"));
			}
		}, Map.of("key1", "val1"), new AutomationDateTool().addDays(new Date(), 1), "0 0/50 0 ? * * *");

		assertTrue(testSubject.getGroups().contains("Test"));
		assertEquals("val1", testSubject.getJob("test", "Test").getJobDataMap().get("key1"));
		assertEquals(2, testSubject.getTriggers("Test", Tenant.DEFAULT_ID).size());
		assertEquals(2, testSubject.getTriggersOfJob("test", "Test").size());
		assertEquals(1, testSubject.getJobs("Test", null).size());
		assertEquals(1, testSubject.getJobs("Test", Tenant.DEFAULT_ID).size());

		testSubject.unscheduleTrigger("test-0 0/50 0 ? * * *", "Test");
		assertEquals(1, testSubject.getTriggersOfJob("test", "Test").size());

		testSubject.unscheduleJob("test", "Test");
		assertFalse(testSubject.getGroups().contains("Test"));
		assertTrue(testSubject.getJobs("Test", null).isEmpty());
	}
}