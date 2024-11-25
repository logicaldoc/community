package com.logicaldoc.util.time;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * A simple class to pause the current thread
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public abstract class Pause {
	
	private Pause() {
		// Empty
	}
	
	public static void doPause(long ms) throws InterruptedException {
		CountDownLatch latch = new CountDownLatch(1);
		if (latch.await(ms, TimeUnit.MILLISECONDS))
			throw new IllegalStateException("Unexpected latch status");
	}
}