package com.logicaldoc.core.threading;

import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * This thread factory acts like <code>Executors.defaultThreadFactory()</code>
 * but gives a name prefix to the new thread.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class NamedThreadFactory implements ThreadFactory {
	private final String baseName;

	private final AtomicInteger threadNum = new AtomicInteger(0);

	public NamedThreadFactory(String baseName) {
		this.baseName = baseName;
	}

	@Override
	public synchronized Thread newThread(Runnable r) {
		Thread t = Executors.defaultThreadFactory().newThread(r);

		t.setName(baseName + "-" + threadNum.getAndIncrement());

		return t;
	}
}