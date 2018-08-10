package com.logicaldoc.gui.common.client.remote;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.FolderController;

import de.novanic.eventservice.client.event.Event;
import de.novanic.eventservice.client.event.listener.RemoteEventListener;

/**
 * A listener to react to server push
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
public class RemoteMessageListener implements RemoteEventListener {

	private static Set<String> moniteredEvents = new HashSet<String>();

	static {
		moniteredEvents.addAll(Arrays.asList("event.changed", "event.renamed", "event.checkedin", "event.checkedout",
				"event.locked", "event.unlocked", "event.immutable", "event.signed", "event.stamped",
				"event.password.protected", "event.password.unprotected", "event.stored", "event.moved",
				"event.deleted", "event.folder.renamed", "event.folder.changed", "event.folder.deleted",
				"event.folder.created", "event.folder.moved", "event.workflowstatus"));
	}

	@Override
	public void apply(Event anEvent) {
		if (anEvent instanceof MessageEvent) {
			final MessageEvent event = (MessageEvent) anEvent;
			if (moniteredEvents.contains(event.getEvent())) {
				if ("event.changed".equals(event.getEvent()) || "event.renamed".equals(event.getEvent())
						|| "event.checkedin".equals(event.getEvent()) || "event.checkedout".equals(event.getEvent())
						|| "event.locked".equals(event.getEvent()) || "event.unlocked".equals(event.getEvent())
						|| "event.immutable".equals(event.getEvent()) || "event.signed".equals(event.getEvent())
						|| "event.stamped".equals(event.getEvent())
						|| "event.password.protected".equals(event.getEvent())
						|| "event.password.unprotected".equals(event.getEvent())
						|| "event.workflowstatus".equals(event.getEvent())) {
					DocumentController.get().modified(event.getDocument());
				} else if ("event.stored".equals(event.getEvent())) {
					DocumentController.get().stored(event.getDocument());
				} else if ("event.moved".equals(event.getEvent())) {
					DocumentController.get().moved(event.getDocument());
				} else if ("event.deleted".equals(event.getEvent())) {
					DocumentController.get().deleted(new GUIDocument[] { event.getDocument() });
				} else if ("event.folder.renamed".equals(event.getEvent())
						|| "event.folder.changed".equals(event.getEvent())) {
					FolderController.get().modified(event.getFolder());
				} else if ("event.folder.deleted".equals(event.getEvent())) {
					FolderController.get().deleted(event.getFolder());
				} else if ("event.folder.created".equals(event.getEvent())) {
					FolderController.get().created(event.getFolder());
				} else if ("event.folder.moved".equals(event.getEvent())) {
					FolderController.get().moved(event.getFolder());
				}
			}
		}
	}
}