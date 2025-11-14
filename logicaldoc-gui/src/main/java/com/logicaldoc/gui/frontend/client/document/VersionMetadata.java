package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.gwt.i18n.client.NumberFormat;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Show metadata of a version
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class VersionMetadata extends Window {

	private static final String LABEL = "label";

	public VersionMetadata(final GUIVersion version) {
		super();

		setTitle(I18N.message("metadata") + " - " + version.getFileName() + "(" + version.getVersion() + ") ");
		setWidth(450);
		setHeight(350);
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.MAXIMIZE_BUTTON, HeaderControls.MINIMIZE_BUTTON,
				HeaderControls.CLOSE_BUTTON);
		setCanDragReposition(true);
		setCanDragResize(true);
		centerInPage();

		// Prepare the records, each one is related to a version's attribute
		ArrayList<MetadataRecord> records = new ArrayList<>();
		records.add(new MetadataRecord(I18N.message("versiondate"), I18N.message("versiondate"),
				I18N.formatDate(version.getVersionDate()), 0));
		records.add(new MetadataRecord(I18N.message("fileversion"), I18N.message("fileversion"),
				version.getFileVersion(), 0));
		records.add(new MetadataRecord(I18N.message("filename"), I18N.message("filename"), version.getFileName(), 0));
		records.add(new MetadataRecord(I18N.message("size"), I18N.message("size"),
				Util.formatSizeW7(version.getFileSize()), 0));
		records.add(new MetadataRecord(I18N.message("pages"), I18N.message("pages"), Util.formatInt(version.getPages()),
				0));
		records.add(new MetadataRecord(I18N.message("username"), I18N.message("username"), version.getUsername(), 0));
		records.add(new MetadataRecord(I18N.message("comment"), I18N.message("comment"), version.getComment(), 0));
		records.add(new MetadataRecord(I18N.message("customid"), I18N.message("customid"), version.getCustomId(), 0));
		records.add(new MetadataRecord(I18N.message("revision"), I18N.message("revision"), version.getRevision(), 0));
		records.add(new MetadataRecord(I18N.message("language"), I18N.message("language"), version.getLanguage(), 0));
		records.add(new MetadataRecord(I18N.message("createdon"), I18N.message("createdon"),
				I18N.formatDate(version.getCreation()), 0));
		records.add(new MetadataRecord(I18N.message("creator"), I18N.message("creator"), version.getCreator(), 0));
		records.add(new MetadataRecord(I18N.message("publishedon"), I18N.message("publishedon"),
				I18N.formatDate(version.getDate()), 0));
		records.add(
				new MetadataRecord(I18N.message("publisher"), I18N.message("publisher"), version.getPublisher(), 0));
		records.add(new MetadataRecord(I18N.message("size"), I18N.message("size"),
				Util.formatSizeBytes(version.getFileSize()), 0));
		records.add(new MetadataRecord(I18N.message("folder"), I18N.message("folder"),
				version.getFolder().getId() == Constants.DOCUMENTS_FOLDERID ? "/" : version.getFolder().getName(), 0));

		records.add(new MetadataRecord(I18N.message("color"), I18N.message("color"), version.getColor(), 0));

		records.add(new MetadataRecord(I18N.message("tags"), I18N.message("tags"), version.getTagsString(), 0));

		records.add(new MetadataRecord(I18N.message("published"), I18N.message("published"),
				version.isPublished() ? I18N.message("yes") : I18N.message("no"), 0));

		records.add(new MetadataRecord(I18N.message("startpublishing"), I18N.message("startpublishing"),
				version.getStartPublishing() != null ? I18N.formatDate(version.getStartPublishing()) : null, 0));

		records.add(new MetadataRecord(I18N.message("stoppublishing"), I18N.message("stoppublishing"),
				version.getStopPublishing() != null ? I18N.formatDate(version.getStopPublishing()) : null, 0));

		records.add(new MetadataRecord(I18N.message("workflowstatus"), I18N.message("workflowstatus"),
				version.getWorkflowStatus(), 0));

		records.add(new MetadataRecord(I18N.message("template"), I18N.message("template"), version.getTemplate(), 0));
		putExtendedAttributes(records, version);

		ListGridField label = new ListGridField(LABEL);
		label.setAutoFitWidth(true);
		label.setAutoFitWidthApproach(AutoFitWidthApproach.VALUE);
		ListGridField val = new ListGridField("val");

		ListGrid listGrid = new ListGrid();
		listGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		listGrid.setCanFreezeFields(false);
		listGrid.setCanGroupBy(false);
		listGrid.setSelectionType(SelectionStyle.SINGLE);
		listGrid.setAutoFetchData(true);
		listGrid.setCanReorderFields(false);
		listGrid.setCanSort(false);
		listGrid.setShowHeader(false);
		listGrid.setData(records.toArray(new ListGridRecord[0]));
		listGrid.setFields(label, val);

		listGrid.addCellContextClickHandler(contextClick -> {
			MetadataRecord selection = (MetadataRecord) contextClick.getRecord();
			if (selection.getAttribute() != null && selection.getAttribute().getType() == GUIAttribute.TYPE_DOCUMENT
					&& selection.getAttribute().getIntValue() != null)
				showContextMenu(selection);
			contextClick.cancel();
		});

		addItem(listGrid);
	}

	/**
	 * Shows the context menu
	 * 
	 * @return the prepared context menu
	 */
	private void showContextMenu(MetadataRecord selection) {
		DocumentService.Instance.get().getAllowedPermissions(Arrays.asList(selection.getAttribute().getIntValue()),
				new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIAccessControlEntry acl) {
						if (acl == null)
							return;

						long docId = selection.getAttribute().getIntValue();

						MenuItem preview = new MenuItem();
						preview.setTitle(I18N.message("preview"));
						preview.setEnabled(acl.isPreview());
						preview.addClickHandler(
								click -> DocumentService.Instance.get().getById(docId, new DefaultAsyncCallback<>() {
									@Override
									public void handleSuccess(GUIDocument doc) {
										new PreviewPopup(doc).show();
									}
								}));

						MenuItem download = new MenuItem();
						download.setTitle(I18N.message("download"));
						download.setEnabled(acl.isDownload());
						download.addClickHandler(click -> DocUtil.download(docId, null));

						MenuItem open = new MenuItem();
						open.setTitle(I18N.message("openinfolder"));
						open.setEnabled(acl.isRead());
						open.addClickHandler(click -> {
							destroy();

							if (com.logicaldoc.gui.common.client.Menu
									.enabled(com.logicaldoc.gui.common.client.Menu.DOCUMENTS))
								DocumentsPanel.get().openInFolder(docId);
						});

						Menu contextMenu = new Menu();
						contextMenu.setItems(preview, download, open);
						contextMenu.showContextMenu();
					}
				});
	}

	private void putExtendedAttributes(ArrayList<MetadataRecord> records, GUIVersion version) {
		List<MetadataRecord> attributeRecords = new ArrayList<>();

		List<String> names = collectAttributeNames(version);

		for (String name : names)
			attributeRecords.add(new MetadataRecord(version.getAttribute(name)));

		attributeRecords.sort(null);
		records.addAll(attributeRecords);
	}

	private List<String> collectAttributeNames(GUIVersion version) {
		List<String> names = new ArrayList<>();

		// Collect all attribute names from version1
		for (GUIAttribute att : version.getAttributes()) {
			if (!names.contains(att.getName()))
				names.add(att.getName());
		}

		return names;
	}

	public class MetadataRecord extends ListGridRecord implements Comparable<MetadataRecord> {
		private int position = 0;

		private GUIAttribute attribute;

		public MetadataRecord(GUIAttribute attribute) {
			this(attribute.getName(), attribute.getDisplayName(), null, attribute.getPosition());
			this.attribute = attribute;
			setVal(extractValue(attribute));
		}

		public MetadataRecord(String name, String label, String val, int position) {
			super();
			this.position = position;
			setName(name);
			setLabel(label);
			setVal(val);
		}

		public String getName() {
			return getAttributeAsString("name");
		}

		public void setName(String name) {
			setAttribute("name", name != null ? name : "");
		}

		public void setLabel(String label) {
			setAttribute(LABEL, label != null ? label : "");
		}

		public void setVal(String val) {
			setAttribute("val", val != null ? val : "");
		}

		public int getPosition() {
			return position;
		}

		public GUIAttribute getAttribute() {
			return attribute;
		}

		public void setAttribute(GUIAttribute attribute) {
			this.attribute = attribute;
		}

		@Override
		public int compareTo(MetadataRecord other) {
			if (position == other.position)
				return getName().compareTo(other.getName());
			else
				return Integer.compare(position, other.position);
		}

		@Override
		public boolean equals(Object other) {
			return super.equals(other);
		}

		@Override
		public int hashCode() {
			return super.hashCode();
		}

		private String extractValue(GUIAttribute attribute) {
			String val = "";
			if ((attribute.getType() == GUIAttribute.TYPE_STRING || attribute.getType() == GUIAttribute.TYPE_USER
					|| attribute.getType() == GUIAttribute.TYPE_DOCUMENT
					|| attribute.getType() == GUIAttribute.TYPE_FOLDER) && attribute.getStringValue() != null) {
				val = attribute.getStringValue();
			} else if (attribute.getType() == GUIAttribute.TYPE_INT && attribute.getValue() != null) {
				val = Long.toString(attribute.getIntValue());
			} else if (attribute.getType() == GUIAttribute.TYPE_DOUBLE && attribute.getValue() != null) {
				val = NumberFormat.getDecimalFormat().format(attribute.getDoubleValue());
			} else if (attribute.getType() == GUIAttribute.TYPE_DATE && attribute.getValue() != null) {
				val = I18N.formatDate(attribute.getDateValue());
			}
			return val;
		}
	}
}