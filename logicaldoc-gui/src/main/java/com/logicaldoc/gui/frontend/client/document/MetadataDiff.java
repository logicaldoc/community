package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.NumberFormat;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Show differences between two versions at metadata level
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MetadataDiff extends Window {

	public MetadataDiff(final GUIVersion version1, final GUIVersion version2) {
		super();

		setTitle(I18N.message("compare") + " " + version1.getVersion() + " > " + version2.getVersion());
		setWidth(450);
		setHeight(350);
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.MINIMIZE_BUTTON,
				HeaderControls.CLOSE_BUTTON);
		setCanDragReposition(true);
		setCanDragResize(true);
		centerInPage();

		// Prepare the records, each one is related to a version's attribute
		ArrayList<DiffRecord> records = new ArrayList<DiffRecord>();
		records.add(new DiffRecord(I18N.message("versiondate"), I18N.message("versiondate"),
				I18N.formatDate(version1.getVersionDate()), I18N.formatDate(version2.getVersionDate()), 0));
		records.add(new DiffRecord(I18N.message("fileversion"), I18N.message("fileversion"), version1.getFileVersion(),
				version2.getFileVersion(), 0));
		records.add(new DiffRecord(I18N.message("filename"), I18N.message("filename"), version1.getFileName(),
				version2.getFileName(), 0));
		records.add(new DiffRecord(I18N.message("size"), I18N.message("size"),
				Util.formatSizeW7(version1.getFileSize()), Util.formatSizeW7(version2.getFileSize()), 0));
		records.add(new DiffRecord(I18N.message("pages"), I18N.message("pages"), Util.formatInt(version1.getPages()),
				Util.formatInt(version2.getPages()), 0));
		records.add(new DiffRecord(I18N.message("username"), I18N.message("username"), version1.getUsername(),
				version2.getUsername(), 0));
		records.add(new DiffRecord(I18N.message("comment"), I18N.message("comment"), version1.getComment(),
				version2.getComment(), 0));
		records.add(new DiffRecord(I18N.message("customid"), I18N.message("customid"), version1.getCustomId(),
				version2.getCustomId(), 0));
		records.add(new DiffRecord(I18N.message("language"), I18N.message("language"), version1.getLanguage(),
				version2.getLanguage(), 0));
		records.add(new DiffRecord(I18N.message("createdon"), I18N.message("createdon"),
				I18N.formatDate(version1.getCreation()), I18N.formatDate(version2.getCreation()), 0));
		records.add(new DiffRecord(I18N.message("creator"), I18N.message("creator"), version1.getCreator(),
				version2.getCreator(), 0));
		records.add(new DiffRecord(I18N.message("publishedon"), I18N.message("publishedon"),
				I18N.formatDate(version1.getDate()), I18N.formatDate(version2.getDate()), 0));
		records.add(new DiffRecord(I18N.message("publisher"), I18N.message("publisher"), version1.getPublisher(),
				version2.getPublisher(), 0));
		records.add(new DiffRecord(I18N.message("size"), I18N.message("size"),
				Util.formatSizeBytes(version1.getFileSize()), Util.formatSizeBytes(version2.getFileSize()), 0));
		records.add(new DiffRecord(I18N.message("folder"), I18N.message("folder"),
				version1.getFolder().getId() == Constants.DOCUMENTS_FOLDERID ? "/" : version1.getFolder().getName(),
				version2.getFolder().getId() == Constants.DOCUMENTS_FOLDERID ? "/" : version2.getFolder().getName(),
				0));

		records.add(new DiffRecord(I18N.message("color"), I18N.message("color"), version1.getColor(),
				version2.getColor(), 0));

		records.add(new DiffRecord(I18N.message("tags"), I18N.message("tags"), version1.getTagsString(),
				version2.getTagsString(), 0));

		records.add(new DiffRecord(I18N.message("published"), I18N.message("published"),
				version1.getPublished() == 1 ? I18N.message("yes") : I18N.message("no"),
				version2.getPublished() == 1 ? I18N.message("yes") : I18N.message("no"), 0));

		records.add(new DiffRecord(I18N.message("startpublishing"), I18N.message("startpublishing"),
				version1.getStartPublishing() != null ? I18N.formatDate(version1.getStartPublishing()) : null,
				version2.getStartPublishing() != null ? I18N.formatDate(version2.getStartPublishing()) : null, 0));

		records.add(new DiffRecord(I18N.message("stoppublishing"), I18N.message("stoppublishing"),
				version1.getStopPublishing() != null ? I18N.formatDate(version1.getStopPublishing()) : null,
				version2.getStopPublishing() != null ? I18N.formatDate(version2.getStopPublishing()) : null, 0));

		records.add(new DiffRecord(I18N.message("workflowstatus"), I18N.message("workflowstatus"),
				version1.getWorkflowStatus(), version2.getWorkflowStatus(), 0));

		records.add(new DiffRecord(I18N.message("template"), I18N.message("template"), version1.getTemplate(),
				version2.getTemplate(), 0));
		printExtendedAttributes(records, version1, version2);

		ListGridField label = new ListGridField("label", " ");
		ListGridField val1 = new ListGridField("val1", version1.getVersion());
		ListGridField val2 = new ListGridField("val2", version2.getVersion());

		ListGrid listGrid = new ListGrid();
		listGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		listGrid.setCanFreezeFields(false);
		listGrid.setCanGroupBy(false);
		listGrid.setAutoFetchData(true);
		listGrid.setCanReorderFields(false);
		listGrid.setCanSort(false);
		listGrid.setData(records.toArray(new ListGridRecord[0]));
		listGrid.setFields(label, val1, val2);
		addItem(listGrid);

		IButton compareContent = new IButton(I18N.message("comparecontent"));
		compareContent.setWidth100();
		compareContent.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				ComparisonWindow diff = new ComparisonWindow(version1, version2);
				diff.show();
			}
		});
		if (Feature.visible(Feature.COMPARISON)) {
			addItem(compareContent);
			compareContent.setDisabled(!Feature.enabled(Feature.COMPARISON));
		}
	}

	private void printExtendedAttributes(ArrayList<DiffRecord> records, GUIVersion version1, GUIVersion version2) {
		DateTimeFormat dateFormat = DateTimeFormat.getFormat(I18N.message("format_date"));
		NumberFormat numberFormat = NumberFormat.getDecimalFormat();

		List<DiffRecord> attributeRecords = new ArrayList<DiffRecord>();

		List<String> names = prepareAttributeNames(version1, version2);

		for (String name : names) {
			addDiffRecord(version1, version2, dateFormat, numberFormat, attributeRecords, name);
		}

		attributeRecords.sort(null);
		records.addAll(attributeRecords);
	}

	private void addDiffRecord(GUIVersion version1, GUIVersion version2, DateTimeFormat dateFormat,
			NumberFormat numberFormat, List<DiffRecord> attributeRecords, String name) {

		String val1 = "";
		String label = null;
		int position = -1;

		GUIAttribute att1 = version1.getAttribute(name);
		if (att1 != null) {
			label = att1.getDisplayName();
			position = att1.getPosition();

			val1 = extractValue(dateFormat, numberFormat, att1);
		}

		GUIAttribute att2 = version2.getAttribute(name);
		String val2 = "";
		if (att2 != null) {
			if (label == null)
				label = att2.getDisplayName();
			if (position == -1)
				position = att2.getPosition();

			val2 = extractValue(dateFormat, numberFormat, att2);
		}

		DiffRecord record = new DiffRecord(name, label, val1, val2, position);
		attributeRecords.add(record);
	}

	private String extractValue(DateTimeFormat dateFormat, NumberFormat numberFormat, GUIAttribute attribute) {
		String val = "";
		if ((attribute.getType() == GUIAttribute.TYPE_STRING || attribute.getType() == GUIAttribute.TYPE_USER)
				&& attribute.getStringValue() != null) {
			val = attribute.getStringValue();
		} else if (attribute.getType() == GUIAttribute.TYPE_INT && attribute.getValue() != null) {
			val = Long.toString(attribute.getIntValue());
		} else if (attribute.getType() == GUIAttribute.TYPE_DOUBLE && attribute.getValue() != null) {
			val = numberFormat.format(attribute.getDoubleValue());
		} else if (attribute.getType() == GUIAttribute.TYPE_DATE && attribute.getValue() != null) {
			val = dateFormat.format(attribute.getDateValue());
		}
		return val;
	}

	private List<String> prepareAttributeNames(GUIVersion version1, GUIVersion version2) {
		List<String> names = new ArrayList<String>();

		// Collect all attribute names from version1
		for (GUIAttribute att : version1.getAttributes()) {
			if (!names.contains(att.getName()))
				names.add(att.getName());
		}
		// Collect all attribute names from version2
		for (GUIAttribute att : version2.getAttributes()) {
			if (!names.contains(att.getName()))
				names.add(att.getName());
		}
		return names;
	}

	public class DiffRecord extends ListGridRecord implements Comparable<DiffRecord> {
		private int position = 0;

		public DiffRecord(String name, String label, String val1, String val2, int position) {
			super();
			this.position = position;
			setName(name);
			setLabel(label);
			setVal1(val1);
			setVal2(val2);
			if (isDifferent()) {
				setName("<b class='diff'>" + getAttribute("name") + "</b>");
				setLabel("<b class='diff'>" + getAttribute("label") + "</b>");
				setVal1("<b class='diff'>" + getAttribute("val1") + "</b>");
				setVal2("<b class='diff'>" + getAttribute("val2") + "</b>");
			}
		}

		public String getName() {
			return getAttributeAsString("name");
		}

		public void setName(String name) {
			setAttribute("name", name != null ? name : "");
		}

		public void setLabel(String label) {
			setAttribute("label", label != null ? label : "");
		}

		public void setVal1(String val1) {
			setAttribute("val1", val1 != null ? val1 : "");
		}

		public void setVal2(String val2) {
			setAttribute("val2", val2 != null ? val2 : "");
		}

		public int getPosition() {
			return position;
		}

		public boolean isDifferent() {
			return !getAttributeAsString("val1").equals(getAttributeAsString("val2"));
		}

		@Override
		public int compareTo(DiffRecord other) {
			if (position == other.position)
				return getName().compareTo(other.getName());
			else
				return Integer.valueOf(position).compareTo(Integer.valueOf(other.position));
		}
	}
}