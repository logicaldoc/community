package com.logicaldoc.gui.common.client.automation;

import org.wisepersist.gwt.ace.client.AceCompletion;
import org.wisepersist.gwt.ace.client.AceCompletionCallback;
import org.wisepersist.gwt.ace.client.AceCompletionProvider;
import org.wisepersist.gwt.ace.client.AceCompletionSnippet;
import org.wisepersist.gwt.ace.client.AceCompletionSnippetSegment;
import org.wisepersist.gwt.ace.client.AceCompletionSnippetSegmentLiteral;
import org.wisepersist.gwt.ace.client.AceCompletionSnippetSegmentTabstopItem;
import org.wisepersist.gwt.ace.client.AceCompletionValue;
import org.wisepersist.gwt.ace.client.AceEditor;
import org.wisepersist.gwt.ace.client.AceEditorCursorPosition;

/**
 * Provides autocompletion hints for the automation script
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
 */
public class AutomationCompletionProvider implements AceCompletionProvider {
	private static final String CONDITION = "$condition";
	private static final String TEMPLATE = "template";
	private static final String AUTOMATION_TOOL = "$AutomationTool";
	private static final String CONTEXT_TOOL = "$ContextTool";
	private static final String VARIABLE = "variable";

	@Override
	public void getProposals(AceEditor editor, AceEditorCursorPosition pos, String prefix,
			AceCompletionCallback callback) {

		callback
		.invokeWithCompletions(new AceCompletion[] {
				new AceCompletionValue("$AITool", "$AITool",  VARIABLE, 10),
				new AceCompletionValue("$RobotTool", "$RobotTool",  VARIABLE, 10),
				new AceCompletionValue("$DocTool", "$DocTool",  VARIABLE, 10),
				new AceCompletionValue("$DateTool", "$DateTool", VARIABLE, 10),
				new AceCompletionValue("$FolderTool", "$FolderTool", VARIABLE, 10),
				new AceCompletionValue("$NumberTool", "$NumberTool", VARIABLE, 10),
				new AceCompletionValue("$SystemTool", "$SystemTool", VARIABLE, 10),
				new AceCompletionValue(CONTEXT_TOOL, CONTEXT_TOOL, VARIABLE, 10),
				new AceCompletionValue("$WorkflowTool", "$WorkflowTool", VARIABLE, 10),
				new AceCompletionValue(AUTOMATION_TOOL, AUTOMATION_TOOL, VARIABLE, 10),
				new AceCompletionValue("$MailTool", "$MailTool", VARIABLE, 10),
				new AceCompletionValue("$ClassTool", "$ClassTool", VARIABLE, 10),
				new AceCompletionValue("$BarcodeTool", "$BarcodeTool", VARIABLE, 10),
				new AceCompletionValue("$StampTool", "$StampTool", VARIABLE, 10),
				new AceCompletionValue("$SignTool", "$SignTool", VARIABLE, 10),
				new AceCompletionValue("$SplitTool", "$SplitTool", VARIABLE, 10),
				new AceCompletionValue(AUTOMATION_TOOL, AUTOMATION_TOOL, VARIABLE, 10),
				new AceCompletionValue(CONTEXT_TOOL, CONTEXT_TOOL, VARIABLE, 10),
				new AceCompletionValue("$ZonalOCRTool", "$ZonalOCRTool", VARIABLE, 10),
				new AceCompletionValue("$WebsocketTool", "$WebsocketTool", VARIABLE, 10),
				new AceCompletionValue("$UserTool", "$UserTool", VARIABLE, 10),
				new AceCompletionValue("$SecurityTool", "$SecurityTool", VARIABLE, 10),
				new AceCompletionValue("$FormTool", "$FormTool", VARIABLE, 10),
				new AceCompletionValue("$ReportTool", "$ReportTool", VARIABLE, 10),
				new AceCompletionValue("$CalendarTool", "$CalendarTool", VARIABLE, 10),
				new AceCompletionValue("$ReadingRequestTool", "$ReadingRequestTool", VARIABLE, 10),
				new AceCompletionValue("$I18N", "$I18N", VARIABLE, 10),
				new AceCompletionValue("$log", "$log", VARIABLE, 10),
				new AceCompletionValue("$systemDictionary", "$systemDictionary", VARIABLE, 10),
				new AceCompletionValue("$CURRENT_DATE", "$CURRENT_DATE", VARIABLE, 10),
				new AceCompletionValue("$dictionary", "$dictionary", VARIABLE, 10),
				new AceCompletionValue("$nl", "$nl", VARIABLE, 10),
				new AceCompletionValue("$tenantId", "$tenantId", VARIABLE, 10),
				
				
				new AceCompletionSnippet("info",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("$log.info( "),
								new AceCompletionSnippetSegmentTabstopItem("$message"),
								new AceCompletionSnippetSegmentLiteral(" )") },
						TEMPLATE, "$log.info(...)", 10),
				new AceCompletionSnippet("error",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("$log.error( "),
								new AceCompletionSnippetSegmentTabstopItem("$message"),
								new AceCompletionSnippetSegmentLiteral(" )") },
						TEMPLATE, "$log.error(...)", 10),
				new AceCompletionSnippet("foreach",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("#foreach("),
								new AceCompletionSnippetSegmentTabstopItem("$item"),
								new AceCompletionSnippetSegmentLiteral(" in "),
								new AceCompletionSnippetSegmentTabstopItem("$collection"),
								new AceCompletionSnippetSegmentLiteral(")\n ## foreach code\n#end") },
						TEMPLATE, "#foreach(<b>$item</b> in <b>$collection</b>) ... #end", 10),
				new AceCompletionSnippet("ifelse",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("#if( "),
								new AceCompletionSnippetSegmentTabstopItem(CONDITION),
								new AceCompletionSnippetSegmentLiteral(
										" )\n ## if code \n#else\n ## else code\n#end") },
						TEMPLATE, "#if( <b>$condition</b> ) ... #else ... #end", 10),
				new AceCompletionSnippet("ifelseif",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("#if( "),
								new AceCompletionSnippetSegmentTabstopItem(CONDITION),
								new AceCompletionSnippetSegmentLiteral(
										" )\n ## if code \n#elseif( "),
								new AceCompletionSnippetSegmentTabstopItem(CONDITION),
								new AceCompletionSnippetSegmentLiteral(
										" )\n ## if code \n#else\n ## else code\n#end") },
						TEMPLATE, "\"#if( <b>$condition</b> ) ... #elseif( <b>$condition</b> ) ... #else ... #end\"", 10),
				new AceCompletionSnippet("set",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("#set ( "),
								new AceCompletionSnippetSegmentTabstopItem("$name"),
								new AceCompletionSnippetSegmentLiteral(" = "),
								new AceCompletionSnippetSegmentTabstopItem("$value"),
								new AceCompletionSnippetSegmentLiteral(" ) ") },
						TEMPLATE, "#set ( <b>$name</b> = <b>$value</b> ) ", 10)

		});
	}
}