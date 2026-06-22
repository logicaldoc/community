package com.logicaldoc.util.time;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;

import org.junit.Test;

import junit.framework.TestCase;

public class DateUtilTest extends TestCase {

    @Test
    public void testFormat() {
        Date date = Date
                .from(LocalDateTime.parse("1992-06-15T12:50:13.027").atZone(ZoneId.systemDefault()).toInstant());

        assertTrue(DateUtil.format(date).startsWith("1992-06-15 12:50:13"));
        assertTrue(DateUtil.formatWithMillis(date).contains(".027"));
    }

    @Test
    public void testParse() {
        Instant instant = Instant.parse("1978-02-15T12:50:13Z");
        Date date = DateUtil.parse("1978-02-15 12:50:13.000 +0000");

        assertEquals(Date.from(instant), date);
        assertNull(DateUtil.parse("pippo"));
    }

    @Test
    public void testTruncateToDay() {
        Instant instant = Instant.parse("1978-02-15T12:50:13Z");

        Date date = DateUtil.truncateToDay(Date.from(instant));
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        assertEquals("1978-02-15 00:00:00", df.format(date));
    }
}