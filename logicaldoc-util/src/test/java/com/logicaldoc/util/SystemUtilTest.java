package com.logicaldoc.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class SystemUtilTest {

    @Test
    public void testPrintEnvironment() {
        String output = SystemUtil.printEnvironment();
        String separator = System.lineSeparator();

        assertTrue(output.contains("java.version"));
        assertTrue(output.startsWith("#System Environment" + separator + "#"));
    }

    @Test
    public void testPrintStackTrace() {
        assertTrue(SystemUtil.printStackTrace().contains(SystemUtilTest.class.getName()));
    }

    @Test
    public void testGetOS() {
        String originalOsName = System.getProperty("os.name");
        try {
            System.setProperty("os.name", "windows");
            assertEquals("win", SystemUtil.getOS());

            System.setProperty("os.name", "macintosh");
            assertEquals("osx", SystemUtil.getOS());

            System.setProperty("os.name", "sunos");
            assertEquals("sol", SystemUtil.getOS());

            System.setProperty("os.name", "Unix");
            assertEquals("uni", SystemUtil.getOS());

            System.setProperty("os.name", "unknown");
            assertEquals("err", SystemUtil.getOS());
        } finally {
            System.setProperty("os.name", originalOsName);
        }
    }
}