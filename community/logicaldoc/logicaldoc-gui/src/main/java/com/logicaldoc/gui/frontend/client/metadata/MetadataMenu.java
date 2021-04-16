package com.logicaldoc.gui.frontend.client.metadata;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIScheme;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.metadata.barcode.BarcodesPanel;
import com.logicaldoc.gui.frontend.client.metadata.form.FormsPanel;
import com.logicaldoc.gui.frontend.client.metadata.stamp.StampsPanel;
import com.logicaldoc.gui.frontend.client.metadata.tag.TagsPanel;
import com.logicaldoc.gui.frontend.client.metadata.template.TemplatesAndAttributesPanel;
import com.logicaldoc.gui.frontend.client.metadata.zonalocr.ZonalOCRPanel;
import com.logicaldoc.gui.frontend.client.services.SchemeService;
import com.logicaldoc.gui.frontend.client.services.TagService;
import com.logicaldoc.gui.frontend.client.workflow.designer.WorkflowDesigner;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the administration document metadata and workflow menu
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class MetadataMenu extends VLayout {

	public MetadataMenu() {
		setMargin(10);
		setMembersMargin(5);
		setOverflow(Overflow.AUTO);

		Button tags = new Button(I18N.message("tags"));
		tags.setWidth100();
		tags.setHeight(25);
		if (Feature.visible(Feature.TAGS_ADMIN)) {
			addMember(tags);
			if (!Feature.enabled(Feature.TAGS_ADMIN)) {
				tags.setDisabled(true);
				tags.setTooltip(I18N.message("featuredisabled"));
			}
		}
		tags.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				TagService.Instance.get().getSettings(new AsyncCallback<GUIParameter[]>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIParameter[] parameters) {
						AdminScreen.get().setContent(new TagsPanel(parameters));
					}
				});
			}
		});

		Button templates = new Button(I18N.message("templates"));
		templates.setWidth100();
		templates.setHeight(25);

		if (Feature.visible(Feature.TEMPLATE)) {
			addMember(templates);
			if (!Feature.enabled(Feature.TEMPLATE)) {
				templates.setDisabled(true);
				templates.setTooltip(I18N.message("featuredisabled"));
			}
		}
		templates.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new TemplatesAndAttributesPanel());
			}
		});

		Button customidAndAutonaming = new Button(I18N.message("customidandnaming"));
		customidAndAutonaming.setWidth100();
		customidAndAutonaming.setHeight(25);

		if (Menu.enabled(Menu.CUSTOM_ID) && (Feature.visible(Feature.CUSTOMID) || Feature.visible(Feature.AUTO_NAMING)
				|| Feature.visible(Feature.AUTO_FOLDING))) {
			addMember(customidAndAutonaming);
			if (!Feature.enabled(Feature.CUSTOMID) && !Feature.enabled(Feature.AUTO_NAMING)
					&& !Feature.enabled(Feature.AUTO_FOLDING)) {
				customidAndAutonaming.setDisabled(true);
				customidAndAutonaming.setTooltip(I18N.message("featuredisabled"));
			}
		}

		customidAndAutonaming.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SchemeService.Instance.get().load(new AsyncCallback<GUIScheme[]>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(final GUIScheme[] schemas) {
						AdminScreen.get().setContent(new CustomIdPanel(schemas));
					}
				});
			}
		});

		Button workflow = new Button(I18N.message("workflow"));
		workflow.setWidth100();
		workflow.setHeight(25);

		if (Feature.visible(Feature.WORKFLOW) && Menu.enabled(Menu.WORKFLOW)) {
			addMember(workflow);
			if (!Feature.enabled(Feature.WORKFLOW)) {
				workflow.setDisabled(true);
				workflow.setTooltip(I18N.message("featuredisabled"));
			}
		}
		workflow.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new WorkflowDesigner(new GUIWorkflow()));
			}
		});

		Button folderTemplates = new Button(I18N.message("foldertemplates"));
		folderTemplates.setWidth100();
		folderTemplates.setHeight(25);
		folderTemplates.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new FolderTemplatesPanel());
			}
		});

		if (Feature.visible(Feature.FOLDER_TEMPLATE)) {
			addMember(folderTemplates);
			if (!Feature.enabled(Feature.FOLDER_TEMPLATE)) {
				folderTemplates.setDisabled(true);
				folderTemplates.setTooltip(I18N.message("featuredisabled"));
			}
		}

		Button retentionPolicies = new Button(I18N.message("retentionpolicies"));
		retentionPolicies.setWidth100();
		retentionPolicies.setHeight(25);

		if (Feature.visible(Feature.RETENTION_POLICIES) && Menu.enabled(Menu.RETENTION_POLICIES)) {
			addMember(retentionPolicies);
			if (!Feature.enabled(Feature.RETENTION_POLICIES)) {
				retentionPolicies.setDisabled(true);
				retentionPolicies.setTooltip(I18N.message("featuredisabled"));
			}
		}
		retentionPolicies.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new RetentionPoliciesPanel());
			}
		});

		Button stamps = new Button(I18N.message("stamps"));
		stamps.setWidth100();
		stamps.setHeight(25);

		if (Feature.visible(Feature.STAMP) && Menu.enabled(Menu.STAMPS)) {
			addMember(stamps);
			if (!Feature.enabled(Feature.STAMP)) {
				stamps.setDisabled(true);
				stamps.setTooltip(I18N.message("featuredisabled"));
			}
		}
		stamps.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new StampsPanel());
			}
		});

		Button forms = new Button(I18N.message("forms"));
		forms.setWidth100();
		forms.setHeight(25);

		if (Feature.visible(Feature.FORM) && Menu.enabled(Menu.FORMS)) {
			addMember(forms);
			if (!Feature.enabled(Feature.FORM)) {
				forms.setDisabled(true);
				forms.setTooltip(I18N.message("featuredisabled"));
			}
		}
		forms.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new FormsPanel());
			}
		});

		Button barcode = new Button(I18N.message("barcodes"));
		barcode.setWidth100();
		barcode.setHeight(25);

		if (Feature.visible(Feature.BARCODES) && Menu.enabled(Menu.BARCODES)) {
			addMember(barcode);
			if (!Feature.enabled(Feature.BARCODES)) {
				barcode.setDisabled(true);
				barcode.setTooltip(I18N.message("featuredisabled"));
			}
		}
		barcode.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new BarcodesPanel());
			}
		});

		Button zonalOcr = new Button(I18N.message("zonalocr"));
		zonalOcr.setWidth100();
		zonalOcr.setHeight(25);

		if (Feature.visible(Feature.ZONAL_OCR) && Menu.enabled(Menu.ZONAL_OCR)) {
			addMember(zonalOcr);
			if (!Feature.enabled(Feature.ZONAL_OCR)) {
				zonalOcr.setDisabled(true);
				zonalOcr.setTooltip(I18N.message("featuredisabled"));
			}
		}
		zonalOcr.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new ZonalOCRPanel(null, null));
			}
		});
	}
}