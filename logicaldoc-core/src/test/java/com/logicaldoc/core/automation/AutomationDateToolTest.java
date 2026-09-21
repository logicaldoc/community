package com.logicaldoc.core.automation;

import static org.junit.Assert.assertEquals;

import java.time.Instant;
import java.util.Date;
import java.util.TimeZone;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;

/**
 * Test case for the <code>AutomationDateTool</code>
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.3.1
 */
public class AutomationDateToolTest extends AbstractCoreTestCase {

    private AutomationDateTool newTool() {
        return new AutomationDateTool("yyyy-MM-dd HH:mm:ss", "yyyy-MM-dd HH:mm:ss.SSS", "yyyy-MM-dd");
    }

    @Test
    public void testFormatISO() {
        Date date = Date.from(Instant.parse("2026-09-18T23:30:45.123Z"));

        assertEquals("2026-09-18T23:30:45", newTool().formatISO(date));
    }

    @Test
    public void testFormatSQL() {
        TimeZone original = TimeZone.getDefault();

        try {
            TimeZone.setDefault(TimeZone.getTimeZone("Europe/Rome"));

            Date date = Date.from(Instant.parse("2026-09-18T23:30:45Z"));

            // In Rome, this timestamp falls on the following day.
            assertEquals("2026-09-19", newTool().formatSQL(date));
        } finally {
            TimeZone.setDefault(original);
        }
    }

    @Test
    public void testNullDates() {
        AutomationDateTool tool = newTool();

        assertEquals("", tool.formatISO(null));
        assertEquals("", tool.formatSQL(null));
    }
}
