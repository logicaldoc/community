package com.logicaldoc.util.time;

import static org.junit.Assert.assertEquals;

import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.time.StopWatch;
import org.junit.Test;

public class PauseTest {

    @Test
    public void testDoPause() throws InterruptedException {
        StopWatch watch = new StopWatch();
        watch.start();
        Pause.doPause(2000);
        watch.stop();

        assertEquals(2L, watch.getTime(TimeUnit.SECONDS));
    }
}