package com.logicaldoc.gui.frontend.client.metadata;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIRetentionPolicy;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.frontend.client.folder.FolderSelector;
import com.logicaldoc.gui.frontend.client.services.RetentionPoliciesService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel collects details about a retention policy
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2
 */
public class RetentionPolicyDetailsPanel extends VLayout implements FolderChangeListener {
	private GUIRetentionPolicy policy;

	private Layout propertiesTabPanel;

	private HLayout savePanel;

	private TabSet tabSet = new TabSet();

	private RetentionPoliciesPanel policiesPanel;

	private DynamicForm form = null;

	private FolderSelector folder = null;

	public RetentionPolicyDetailsPanel(RetentionPoliciesPanel policiesPanel) {
		super();

		this.policiesPanel = policiesPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		savePanel = new HLayout();
		savePanel.setHeight(20);
		savePanel.setVisible(false);
		savePanel.setStyleName("warn");
		savePanel.setWidth100();
		Button saveButton = new Button(I18N.message("save"));
		saveButton.setAutoFit(true);
		saveButton.setMargin(2);
		saveButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		saveButton.setLayoutAlign(VerticalAlignment.CENTER);

		HTMLPane spacer = new HTMLPane();
		spacer.setContents("<div>&nbsp;</div>");
		spacer.setWidth("70%");
		spacer.setOverflow(Overflow.HIDDEN);

		Img closeImage = ItemFactory.newImgIcon("delete.png");
		closeImage.setHeight("16px");
		closeImage.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (policy.getId() != 0) {
					RetentionPoliciesService.Instance.get().getPolicy(policy.getId(),
							new AsyncCallback<GUIRetentionPolicy>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(GUIRetentionPolicy policy) {
									setPolicy(policy);
								}
							});
				} else {
					GUIRetentionPolicy newsPolicy = new GUIRetentionPolicy();
					setPolicy(newsPolicy);
				}
				savePanel.setVisible(false);
			}
		});
		closeImage.setCursor(Cursor.HAND);
		closeImage.setTooltip(I18N.message("close"));
		closeImage.setLayoutAlign(Alignment.RIGHT);
		closeImage.setLayoutAlign(VerticalAlignment.CENTER);

		savePanel.addMember(saveButton);
		savePanel.addMember(spacer);
		savePanel.addMember(closeImage);
		addMember(savePanel);
	}

	private void refresh() {
		if (savePanel != null)
			savePanel.setVisible(false);

		/*
		 * Prepare the standard properties tab
		 */
		if (tabSet != null)
			removeMember(tabSet);

		tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		addMember(tabSet);

		ChangedHandler changedHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};

		form = new DynamicForm();
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem name = ItemFactory.newSimpleTextItem("name", "name", policy.getName());
		name.addChangedHandler(changedHandler);
		name.setWidth(200);
		name.setRequired(true);

		IntegerItem days = ItemFactory.newIntegerItem("days", "retentiondays", policy.getRetentionDays());
		days.addChangedHandler(changedHandler);
		days.setWidth(100);
		days.setRequired(true);

		SelectItem dateOption = ItemFactory.newRetentionDateOption(policy.getDateOption());
		dateOption.addChangedHandler(changedHandler);

		SelectItem template = ItemFactory.newTemplateSelector(true, policy.getTemplateId());
		template.addChangedHandler(changedHandler);

		folder = new FolderSelector(null, true);
		folder.setFolder(policy.getFolderId(), policy.getFolderName());
		folder.addFolderChangeListener(RetentionPolicyDetailsPanel.this);

		SelectItem action = ItemFactory.newRetentionAction(policy.getAction());
		action.addChangedHandler(changedHandler);

		form.setItems(name, days, dateOption, template, action, folder);

		propertiesTab.setPane(form);
	}

	public GUIRetentionPolicy getPolicy() {
		return policy;
	}

	public void setPolicy(GUIRetentionPolicy policy) {
		this.policy = policy;
		refresh();
	}

	public void onModified() {
		savePanel.setVisible(true);
	}

	public void onSave() {
		if (form.validate()) {
			policy.setName(form.getValueAsString("name"));
			policy.setRetentionDays(Integer.parseInt(form.getValueAsString("days")));
			policy.setDateOption(Integer.parseInt(form.getValueAsString("dateoption")));
			policy.setAction(Integer.parseInt(form.getValueAsString("action")));
			if (form.getValue("template") == null || "".equals(form.getValueAsString("template")))
				policy.setTemplateId(null);
			else
				policy.setTemplateId(Long.parseLong(form.getValueAsString("template")));

			if (folder == null || folder.getFolderId() == null)
				policy.setFolderId(null);
			else
				policy.setFolderId(folder.getFolderId());

			RetentionPoliciesService.Instance.get().save(policy, new AsyncCallback<GUIRetentionPolicy>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIRetentionPolicy newPolicy) {
					savePanel.setVisible(false);
					if (newPolicy != null) {
						policiesPanel.updateRecord(newPolicy);
						policiesPanel.showPolicyDetails(newPolicy);
					}
					if (policy.getId() == 0L)
						policiesPanel.refresh();
				}
			});
		}
	}

	@Override
	public void onChanged(GUIFolder folder) {
		onModified();
	}
}