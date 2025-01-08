package com.logicaldoc.gui.frontend.client.clipboard;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This represent a clipboard of documents the user is wotking on
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Clipboard extends HashSet<GUIDocument> {

	private static final long serialVersionUID = 1L;

	public static final String COPY = "copy";

	public static final String CUT = "cut";

	private static Clipboard instance = new Clipboard();

	private transient Set<ClipboardObserver> observers = new HashSet<>();

	private String lastAction = COPY;

	public String getLastAction() {
		return lastAction;
	}

	public void setLastAction(String lastAction) {
		this.lastAction = lastAction;
	}

	private Clipboard() {
		super();
	}

	public static Clipboard getInstance() {
		return instance;
	}

	public void addObserver(ClipboardObserver observer) {
		observers.add(observer);
	}

	@Override
	public boolean add(GUIDocument e) {
		if (super.add(e)) {
			for (ClipboardObserver observer : observers) {
				observer.onAdd(e);
			}
			return true;
		}
		return false;
	}

	@Override
	public void clear() {
		super.clear();
		for (ClipboardObserver observer : observers) {
			observer.onRemove(null);
		}
	}

	@Override
	public boolean remove(Object o) {
		if (super.remove(o)) {
			for (ClipboardObserver observer : observers) {
				observer.onRemove((GUIDocument) o);
			}
			return true;
		}
		return false;
	}

	public ListGridRecord[] getRecords() {
		ListGridRecord[] array = new ListGridRecord[size()];
		int i = 0;
		for (GUIDocument document : this) {
			array[i] = new ListGridRecord();
			array[i].setAttribute("id", Long.toString(document.getId()));
			array[i].setAttribute("filename", document.getFileName());
			array[i].setAttribute("icon", document.getIcon());
			i++;
		}
		return array;
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}