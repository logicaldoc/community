package com.logicaldoc.webservice;

public class GZIPOutInterceptor extends org.apache.cxf.transport.common.gzip.GZIPOutInterceptor {

	@Override
	public void setThreshold(int threshold) {
		if (threshold >= 0)
			super.setThreshold(threshold);
		else
			super.setThreshold(999999999);
	}
}
