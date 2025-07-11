package com.logicaldoc.util.time;

import static org.junit.Assert.assertEquals;

import java.util.Date;

import org.junit.Test;

import com.logicaldoc.util.time.TimeDiff.TimeField;

public class PauseTest {

	@Test
	public void testDoPause() throws InterruptedException {
		Date date1 = new Date();
		Pause.doPause(2000);
		Date date2 = new Date();

		assertEquals(2L, TimeDiff.getTimeDifference(date1, date2, TimeField.SECOND));
	}
}