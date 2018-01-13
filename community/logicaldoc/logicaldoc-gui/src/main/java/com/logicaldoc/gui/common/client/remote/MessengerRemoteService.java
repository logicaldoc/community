package com.logicaldoc.gui.common.client.remote;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;

import de.novanic.eventservice.client.event.domain.Domain;
import de.novanic.eventservice.client.event.domain.DomainFactory;

@RemoteServiceRelativePath("MessengerRemoteService")
public interface MessengerRemoteService extends RemoteService {
	
	public static final Domain MESSAGE_DOMAIN_GUI = DomainFactory.getDomain("message_domain_gui");

	public void start();

	public static class Instance {
		private static MessengerRemoteServiceAsync instance;

		public static MessengerRemoteServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(MessengerRemoteService.class);
			}
			return instance;
		}
	}
}