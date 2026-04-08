package no.ntnu.tdt4250.rf.ide.contentassist.antlr.internal;

import java.io.InputStream;
import org.eclipse.xtext.*;
import org.eclipse.xtext.parser.*;
import org.eclipse.xtext.parser.impl.*;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.parser.antlr.XtextTokenStream;
import org.eclipse.xtext.parser.antlr.XtextTokenStream.HiddenTokens;
import org.eclipse.xtext.ide.editor.contentassist.antlr.internal.AbstractInternalContentAssistParser;
import org.eclipse.xtext.ide.editor.contentassist.antlr.internal.DFA;
import no.ntnu.tdt4250.rf.services.RealmForgeDslGrammarAccess;



import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

@SuppressWarnings("all")
public class InternalRealmForgeDslParser extends AbstractInternalContentAssistParser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "RULE_STRING", "RULE_ID", "RULE_INT", "RULE_ML_COMMENT", "RULE_SL_COMMENT", "RULE_WS", "RULE_ANY_OTHER", "'E'", "'e'", "'EASY'", "'NORMAL'", "'HARD'", "'name'", "'description'", "'unit'", "'-'", "'.'", "'{'", "'}'", "'message'", "'TypeRacerEvent'", "'difficulty'", "'retries'", "'sentence'", "'result'", "'timeLimit'", "'QuestionEvent'", "'question'", "'options'", "','", "'text'", "'isCaseSensitive'", "'isCorrectAnswer'"
    };
    public static final int RULE_STRING=4;
    public static final int RULE_SL_COMMENT=8;
    public static final int T__19=19;
    public static final int T__15=15;
    public static final int T__16=16;
    public static final int T__17=17;
    public static final int T__18=18;
    public static final int T__11=11;
    public static final int T__33=33;
    public static final int T__12=12;
    public static final int T__34=34;
    public static final int T__13=13;
    public static final int T__35=35;
    public static final int T__14=14;
    public static final int T__36=36;
    public static final int EOF=-1;
    public static final int T__30=30;
    public static final int T__31=31;
    public static final int T__32=32;
    public static final int RULE_ID=5;
    public static final int RULE_WS=9;
    public static final int RULE_ANY_OTHER=10;
    public static final int T__26=26;
    public static final int T__27=27;
    public static final int T__28=28;
    public static final int RULE_INT=6;
    public static final int T__29=29;
    public static final int T__22=22;
    public static final int RULE_ML_COMMENT=7;
    public static final int T__23=23;
    public static final int T__24=24;
    public static final int T__25=25;
    public static final int T__20=20;
    public static final int T__21=21;

    // delegates
    // delegators


        public InternalRealmForgeDslParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public InternalRealmForgeDslParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        

    public String[] getTokenNames() { return InternalRealmForgeDslParser.tokenNames; }
    public String getGrammarFileName() { return "InternalRealmForgeDsl.g"; }


    	private RealmForgeDslGrammarAccess grammarAccess;

    	public void setGrammarAccess(RealmForgeDslGrammarAccess grammarAccess) {
    		this.grammarAccess = grammarAccess;
    	}

    	@Override
    	protected Grammar getGrammar() {
    		return grammarAccess.getGrammar();
    	}

    	@Override
    	protected String getValueForTokenName(String tokenName) {
    		return tokenName;
    	}



    // $ANTLR start "entryRuleEventPack"
    // InternalRealmForgeDsl.g:53:1: entryRuleEventPack : ruleEventPack EOF ;
    public final void entryRuleEventPack() throws RecognitionException {
        try {
            // InternalRealmForgeDsl.g:54:1: ( ruleEventPack EOF )
            // InternalRealmForgeDsl.g:55:1: ruleEventPack EOF
            {
             before(grammarAccess.getEventPackRule()); 
            pushFollow(FOLLOW_1);
            ruleEventPack();

            state._fsp--;

             after(grammarAccess.getEventPackRule()); 
            match(input,EOF,FOLLOW_2); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleEventPack"


    // $ANTLR start "ruleEventPack"
    // InternalRealmForgeDsl.g:62:1: ruleEventPack : ( ( rule__EventPack__Group__0 ) ) ;
    public final void ruleEventPack() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:66:2: ( ( ( rule__EventPack__Group__0 ) ) )
            // InternalRealmForgeDsl.g:67:2: ( ( rule__EventPack__Group__0 ) )
            {
            // InternalRealmForgeDsl.g:67:2: ( ( rule__EventPack__Group__0 ) )
            // InternalRealmForgeDsl.g:68:3: ( rule__EventPack__Group__0 )
            {
             before(grammarAccess.getEventPackAccess().getGroup()); 
            // InternalRealmForgeDsl.g:69:3: ( rule__EventPack__Group__0 )
            // InternalRealmForgeDsl.g:69:4: rule__EventPack__Group__0
            {
            pushFollow(FOLLOW_2);
            rule__EventPack__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getEventPackAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleEventPack"


    // $ANTLR start "entryRuleEvent"
    // InternalRealmForgeDsl.g:78:1: entryRuleEvent : ruleEvent EOF ;
    public final void entryRuleEvent() throws RecognitionException {
        try {
            // InternalRealmForgeDsl.g:79:1: ( ruleEvent EOF )
            // InternalRealmForgeDsl.g:80:1: ruleEvent EOF
            {
             before(grammarAccess.getEventRule()); 
            pushFollow(FOLLOW_1);
            ruleEvent();

            state._fsp--;

             after(grammarAccess.getEventRule()); 
            match(input,EOF,FOLLOW_2); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleEvent"


    // $ANTLR start "ruleEvent"
    // InternalRealmForgeDsl.g:87:1: ruleEvent : ( ( rule__Event__Alternatives ) ) ;
    public final void ruleEvent() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:91:2: ( ( ( rule__Event__Alternatives ) ) )
            // InternalRealmForgeDsl.g:92:2: ( ( rule__Event__Alternatives ) )
            {
            // InternalRealmForgeDsl.g:92:2: ( ( rule__Event__Alternatives ) )
            // InternalRealmForgeDsl.g:93:3: ( rule__Event__Alternatives )
            {
             before(grammarAccess.getEventAccess().getAlternatives()); 
            // InternalRealmForgeDsl.g:94:3: ( rule__Event__Alternatives )
            // InternalRealmForgeDsl.g:94:4: rule__Event__Alternatives
            {
            pushFollow(FOLLOW_2);
            rule__Event__Alternatives();

            state._fsp--;


            }

             after(grammarAccess.getEventAccess().getAlternatives()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleEvent"


    // $ANTLR start "entryRuleEString"
    // InternalRealmForgeDsl.g:103:1: entryRuleEString : ruleEString EOF ;
    public final void entryRuleEString() throws RecognitionException {
        try {
            // InternalRealmForgeDsl.g:104:1: ( ruleEString EOF )
            // InternalRealmForgeDsl.g:105:1: ruleEString EOF
            {
             before(grammarAccess.getEStringRule()); 
            pushFollow(FOLLOW_1);
            ruleEString();

            state._fsp--;

             after(grammarAccess.getEStringRule()); 
            match(input,EOF,FOLLOW_2); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleEString"


    // $ANTLR start "ruleEString"
    // InternalRealmForgeDsl.g:112:1: ruleEString : ( ( rule__EString__Alternatives ) ) ;
    public final void ruleEString() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:116:2: ( ( ( rule__EString__Alternatives ) ) )
            // InternalRealmForgeDsl.g:117:2: ( ( rule__EString__Alternatives ) )
            {
            // InternalRealmForgeDsl.g:117:2: ( ( rule__EString__Alternatives ) )
            // InternalRealmForgeDsl.g:118:3: ( rule__EString__Alternatives )
            {
             before(grammarAccess.getEStringAccess().getAlternatives()); 
            // InternalRealmForgeDsl.g:119:3: ( rule__EString__Alternatives )
            // InternalRealmForgeDsl.g:119:4: rule__EString__Alternatives
            {
            pushFollow(FOLLOW_2);
            rule__EString__Alternatives();

            state._fsp--;


            }

             after(grammarAccess.getEStringAccess().getAlternatives()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleEString"


    // $ANTLR start "entryRuleEDoubleObject"
    // InternalRealmForgeDsl.g:128:1: entryRuleEDoubleObject : ruleEDoubleObject EOF ;
    public final void entryRuleEDoubleObject() throws RecognitionException {
        try {
            // InternalRealmForgeDsl.g:129:1: ( ruleEDoubleObject EOF )
            // InternalRealmForgeDsl.g:130:1: ruleEDoubleObject EOF
            {
             before(grammarAccess.getEDoubleObjectRule()); 
            pushFollow(FOLLOW_1);
            ruleEDoubleObject();

            state._fsp--;

             after(grammarAccess.getEDoubleObjectRule()); 
            match(input,EOF,FOLLOW_2); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleEDoubleObject"


    // $ANTLR start "ruleEDoubleObject"
    // InternalRealmForgeDsl.g:137:1: ruleEDoubleObject : ( ( rule__EDoubleObject__Group__0 ) ) ;
    public final void ruleEDoubleObject() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:141:2: ( ( ( rule__EDoubleObject__Group__0 ) ) )
            // InternalRealmForgeDsl.g:142:2: ( ( rule__EDoubleObject__Group__0 ) )
            {
            // InternalRealmForgeDsl.g:142:2: ( ( rule__EDoubleObject__Group__0 ) )
            // InternalRealmForgeDsl.g:143:3: ( rule__EDoubleObject__Group__0 )
            {
             before(grammarAccess.getEDoubleObjectAccess().getGroup()); 
            // InternalRealmForgeDsl.g:144:3: ( rule__EDoubleObject__Group__0 )
            // InternalRealmForgeDsl.g:144:4: rule__EDoubleObject__Group__0
            {
            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getEDoubleObjectAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleEDoubleObject"


    // $ANTLR start "entryRuleEInt"
    // InternalRealmForgeDsl.g:153:1: entryRuleEInt : ruleEInt EOF ;
    public final void entryRuleEInt() throws RecognitionException {
        try {
            // InternalRealmForgeDsl.g:154:1: ( ruleEInt EOF )
            // InternalRealmForgeDsl.g:155:1: ruleEInt EOF
            {
             before(grammarAccess.getEIntRule()); 
            pushFollow(FOLLOW_1);
            ruleEInt();

            state._fsp--;

             after(grammarAccess.getEIntRule()); 
            match(input,EOF,FOLLOW_2); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleEInt"


    // $ANTLR start "ruleEInt"
    // InternalRealmForgeDsl.g:162:1: ruleEInt : ( ( rule__EInt__Group__0 ) ) ;
    public final void ruleEInt() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:166:2: ( ( ( rule__EInt__Group__0 ) ) )
            // InternalRealmForgeDsl.g:167:2: ( ( rule__EInt__Group__0 ) )
            {
            // InternalRealmForgeDsl.g:167:2: ( ( rule__EInt__Group__0 ) )
            // InternalRealmForgeDsl.g:168:3: ( rule__EInt__Group__0 )
            {
             before(grammarAccess.getEIntAccess().getGroup()); 
            // InternalRealmForgeDsl.g:169:3: ( rule__EInt__Group__0 )
            // InternalRealmForgeDsl.g:169:4: rule__EInt__Group__0
            {
            pushFollow(FOLLOW_2);
            rule__EInt__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getEIntAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleEInt"


    // $ANTLR start "entryRuleEventResult"
    // InternalRealmForgeDsl.g:178:1: entryRuleEventResult : ruleEventResult EOF ;
    public final void entryRuleEventResult() throws RecognitionException {
        try {
            // InternalRealmForgeDsl.g:179:1: ( ruleEventResult EOF )
            // InternalRealmForgeDsl.g:180:1: ruleEventResult EOF
            {
             before(grammarAccess.getEventResultRule()); 
            pushFollow(FOLLOW_1);
            ruleEventResult();

            state._fsp--;

             after(grammarAccess.getEventResultRule()); 
            match(input,EOF,FOLLOW_2); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleEventResult"


    // $ANTLR start "ruleEventResult"
    // InternalRealmForgeDsl.g:187:1: ruleEventResult : ( ( rule__EventResult__Group__0 ) ) ;
    public final void ruleEventResult() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:191:2: ( ( ( rule__EventResult__Group__0 ) ) )
            // InternalRealmForgeDsl.g:192:2: ( ( rule__EventResult__Group__0 ) )
            {
            // InternalRealmForgeDsl.g:192:2: ( ( rule__EventResult__Group__0 ) )
            // InternalRealmForgeDsl.g:193:3: ( rule__EventResult__Group__0 )
            {
             before(grammarAccess.getEventResultAccess().getGroup()); 
            // InternalRealmForgeDsl.g:194:3: ( rule__EventResult__Group__0 )
            // InternalRealmForgeDsl.g:194:4: rule__EventResult__Group__0
            {
            pushFollow(FOLLOW_2);
            rule__EventResult__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getEventResultAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleEventResult"


    // $ANTLR start "entryRuleTypeRacerEvent"
    // InternalRealmForgeDsl.g:203:1: entryRuleTypeRacerEvent : ruleTypeRacerEvent EOF ;
    public final void entryRuleTypeRacerEvent() throws RecognitionException {
        try {
            // InternalRealmForgeDsl.g:204:1: ( ruleTypeRacerEvent EOF )
            // InternalRealmForgeDsl.g:205:1: ruleTypeRacerEvent EOF
            {
             before(grammarAccess.getTypeRacerEventRule()); 
            pushFollow(FOLLOW_1);
            ruleTypeRacerEvent();

            state._fsp--;

             after(grammarAccess.getTypeRacerEventRule()); 
            match(input,EOF,FOLLOW_2); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleTypeRacerEvent"


    // $ANTLR start "ruleTypeRacerEvent"
    // InternalRealmForgeDsl.g:212:1: ruleTypeRacerEvent : ( ( rule__TypeRacerEvent__Group__0 ) ) ;
    public final void ruleTypeRacerEvent() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:216:2: ( ( ( rule__TypeRacerEvent__Group__0 ) ) )
            // InternalRealmForgeDsl.g:217:2: ( ( rule__TypeRacerEvent__Group__0 ) )
            {
            // InternalRealmForgeDsl.g:217:2: ( ( rule__TypeRacerEvent__Group__0 ) )
            // InternalRealmForgeDsl.g:218:3: ( rule__TypeRacerEvent__Group__0 )
            {
             before(grammarAccess.getTypeRacerEventAccess().getGroup()); 
            // InternalRealmForgeDsl.g:219:3: ( rule__TypeRacerEvent__Group__0 )
            // InternalRealmForgeDsl.g:219:4: rule__TypeRacerEvent__Group__0
            {
            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getTypeRacerEventAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleTypeRacerEvent"


    // $ANTLR start "entryRuleQuestionEvent"
    // InternalRealmForgeDsl.g:228:1: entryRuleQuestionEvent : ruleQuestionEvent EOF ;
    public final void entryRuleQuestionEvent() throws RecognitionException {
        try {
            // InternalRealmForgeDsl.g:229:1: ( ruleQuestionEvent EOF )
            // InternalRealmForgeDsl.g:230:1: ruleQuestionEvent EOF
            {
             before(grammarAccess.getQuestionEventRule()); 
            pushFollow(FOLLOW_1);
            ruleQuestionEvent();

            state._fsp--;

             after(grammarAccess.getQuestionEventRule()); 
            match(input,EOF,FOLLOW_2); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleQuestionEvent"


    // $ANTLR start "ruleQuestionEvent"
    // InternalRealmForgeDsl.g:237:1: ruleQuestionEvent : ( ( rule__QuestionEvent__Group__0 ) ) ;
    public final void ruleQuestionEvent() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:241:2: ( ( ( rule__QuestionEvent__Group__0 ) ) )
            // InternalRealmForgeDsl.g:242:2: ( ( rule__QuestionEvent__Group__0 ) )
            {
            // InternalRealmForgeDsl.g:242:2: ( ( rule__QuestionEvent__Group__0 ) )
            // InternalRealmForgeDsl.g:243:3: ( rule__QuestionEvent__Group__0 )
            {
             before(grammarAccess.getQuestionEventAccess().getGroup()); 
            // InternalRealmForgeDsl.g:244:3: ( rule__QuestionEvent__Group__0 )
            // InternalRealmForgeDsl.g:244:4: rule__QuestionEvent__Group__0
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getQuestionEventAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleQuestionEvent"


    // $ANTLR start "entryRuleOption"
    // InternalRealmForgeDsl.g:253:1: entryRuleOption : ruleOption EOF ;
    public final void entryRuleOption() throws RecognitionException {
        try {
            // InternalRealmForgeDsl.g:254:1: ( ruleOption EOF )
            // InternalRealmForgeDsl.g:255:1: ruleOption EOF
            {
             before(grammarAccess.getOptionRule()); 
            pushFollow(FOLLOW_1);
            ruleOption();

            state._fsp--;

             after(grammarAccess.getOptionRule()); 
            match(input,EOF,FOLLOW_2); 

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "entryRuleOption"


    // $ANTLR start "ruleOption"
    // InternalRealmForgeDsl.g:262:1: ruleOption : ( ( rule__Option__Group__0 ) ) ;
    public final void ruleOption() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:266:2: ( ( ( rule__Option__Group__0 ) ) )
            // InternalRealmForgeDsl.g:267:2: ( ( rule__Option__Group__0 ) )
            {
            // InternalRealmForgeDsl.g:267:2: ( ( rule__Option__Group__0 ) )
            // InternalRealmForgeDsl.g:268:3: ( rule__Option__Group__0 )
            {
             before(grammarAccess.getOptionAccess().getGroup()); 
            // InternalRealmForgeDsl.g:269:3: ( rule__Option__Group__0 )
            // InternalRealmForgeDsl.g:269:4: rule__Option__Group__0
            {
            pushFollow(FOLLOW_2);
            rule__Option__Group__0();

            state._fsp--;


            }

             after(grammarAccess.getOptionAccess().getGroup()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleOption"


    // $ANTLR start "ruleDifficulty"
    // InternalRealmForgeDsl.g:278:1: ruleDifficulty : ( ( rule__Difficulty__Alternatives ) ) ;
    public final void ruleDifficulty() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:282:1: ( ( ( rule__Difficulty__Alternatives ) ) )
            // InternalRealmForgeDsl.g:283:2: ( ( rule__Difficulty__Alternatives ) )
            {
            // InternalRealmForgeDsl.g:283:2: ( ( rule__Difficulty__Alternatives ) )
            // InternalRealmForgeDsl.g:284:3: ( rule__Difficulty__Alternatives )
            {
             before(grammarAccess.getDifficultyAccess().getAlternatives()); 
            // InternalRealmForgeDsl.g:285:3: ( rule__Difficulty__Alternatives )
            // InternalRealmForgeDsl.g:285:4: rule__Difficulty__Alternatives
            {
            pushFollow(FOLLOW_2);
            rule__Difficulty__Alternatives();

            state._fsp--;


            }

             after(grammarAccess.getDifficultyAccess().getAlternatives()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "ruleDifficulty"


    // $ANTLR start "rule__Event__Alternatives"
    // InternalRealmForgeDsl.g:293:1: rule__Event__Alternatives : ( ( ruleTypeRacerEvent ) | ( ruleQuestionEvent ) );
    public final void rule__Event__Alternatives() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:297:1: ( ( ruleTypeRacerEvent ) | ( ruleQuestionEvent ) )
            int alt1=2;
            int LA1_0 = input.LA(1);

            if ( (LA1_0==24) ) {
                alt1=1;
            }
            else if ( (LA1_0==30) ) {
                alt1=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 1, 0, input);

                throw nvae;
            }
            switch (alt1) {
                case 1 :
                    // InternalRealmForgeDsl.g:298:2: ( ruleTypeRacerEvent )
                    {
                    // InternalRealmForgeDsl.g:298:2: ( ruleTypeRacerEvent )
                    // InternalRealmForgeDsl.g:299:3: ruleTypeRacerEvent
                    {
                     before(grammarAccess.getEventAccess().getTypeRacerEventParserRuleCall_0()); 
                    pushFollow(FOLLOW_2);
                    ruleTypeRacerEvent();

                    state._fsp--;

                     after(grammarAccess.getEventAccess().getTypeRacerEventParserRuleCall_0()); 

                    }


                    }
                    break;
                case 2 :
                    // InternalRealmForgeDsl.g:304:2: ( ruleQuestionEvent )
                    {
                    // InternalRealmForgeDsl.g:304:2: ( ruleQuestionEvent )
                    // InternalRealmForgeDsl.g:305:3: ruleQuestionEvent
                    {
                     before(grammarAccess.getEventAccess().getQuestionEventParserRuleCall_1()); 
                    pushFollow(FOLLOW_2);
                    ruleQuestionEvent();

                    state._fsp--;

                     after(grammarAccess.getEventAccess().getQuestionEventParserRuleCall_1()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Event__Alternatives"


    // $ANTLR start "rule__EString__Alternatives"
    // InternalRealmForgeDsl.g:314:1: rule__EString__Alternatives : ( ( RULE_STRING ) | ( RULE_ID ) );
    public final void rule__EString__Alternatives() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:318:1: ( ( RULE_STRING ) | ( RULE_ID ) )
            int alt2=2;
            int LA2_0 = input.LA(1);

            if ( (LA2_0==RULE_STRING) ) {
                alt2=1;
            }
            else if ( (LA2_0==RULE_ID) ) {
                alt2=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }
            switch (alt2) {
                case 1 :
                    // InternalRealmForgeDsl.g:319:2: ( RULE_STRING )
                    {
                    // InternalRealmForgeDsl.g:319:2: ( RULE_STRING )
                    // InternalRealmForgeDsl.g:320:3: RULE_STRING
                    {
                     before(grammarAccess.getEStringAccess().getSTRINGTerminalRuleCall_0()); 
                    match(input,RULE_STRING,FOLLOW_2); 
                     after(grammarAccess.getEStringAccess().getSTRINGTerminalRuleCall_0()); 

                    }


                    }
                    break;
                case 2 :
                    // InternalRealmForgeDsl.g:325:2: ( RULE_ID )
                    {
                    // InternalRealmForgeDsl.g:325:2: ( RULE_ID )
                    // InternalRealmForgeDsl.g:326:3: RULE_ID
                    {
                     before(grammarAccess.getEStringAccess().getIDTerminalRuleCall_1()); 
                    match(input,RULE_ID,FOLLOW_2); 
                     after(grammarAccess.getEStringAccess().getIDTerminalRuleCall_1()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EString__Alternatives"


    // $ANTLR start "rule__EDoubleObject__Alternatives_4_0"
    // InternalRealmForgeDsl.g:335:1: rule__EDoubleObject__Alternatives_4_0 : ( ( 'E' ) | ( 'e' ) );
    public final void rule__EDoubleObject__Alternatives_4_0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:339:1: ( ( 'E' ) | ( 'e' ) )
            int alt3=2;
            int LA3_0 = input.LA(1);

            if ( (LA3_0==11) ) {
                alt3=1;
            }
            else if ( (LA3_0==12) ) {
                alt3=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 3, 0, input);

                throw nvae;
            }
            switch (alt3) {
                case 1 :
                    // InternalRealmForgeDsl.g:340:2: ( 'E' )
                    {
                    // InternalRealmForgeDsl.g:340:2: ( 'E' )
                    // InternalRealmForgeDsl.g:341:3: 'E'
                    {
                     before(grammarAccess.getEDoubleObjectAccess().getEKeyword_4_0_0()); 
                    match(input,11,FOLLOW_2); 
                     after(grammarAccess.getEDoubleObjectAccess().getEKeyword_4_0_0()); 

                    }


                    }
                    break;
                case 2 :
                    // InternalRealmForgeDsl.g:346:2: ( 'e' )
                    {
                    // InternalRealmForgeDsl.g:346:2: ( 'e' )
                    // InternalRealmForgeDsl.g:347:3: 'e'
                    {
                     before(grammarAccess.getEDoubleObjectAccess().getEKeyword_4_0_1()); 
                    match(input,12,FOLLOW_2); 
                     after(grammarAccess.getEDoubleObjectAccess().getEKeyword_4_0_1()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Alternatives_4_0"


    // $ANTLR start "rule__Difficulty__Alternatives"
    // InternalRealmForgeDsl.g:356:1: rule__Difficulty__Alternatives : ( ( ( 'EASY' ) ) | ( ( 'NORMAL' ) ) | ( ( 'HARD' ) ) );
    public final void rule__Difficulty__Alternatives() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:360:1: ( ( ( 'EASY' ) ) | ( ( 'NORMAL' ) ) | ( ( 'HARD' ) ) )
            int alt4=3;
            switch ( input.LA(1) ) {
            case 13:
                {
                alt4=1;
                }
                break;
            case 14:
                {
                alt4=2;
                }
                break;
            case 15:
                {
                alt4=3;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 4, 0, input);

                throw nvae;
            }

            switch (alt4) {
                case 1 :
                    // InternalRealmForgeDsl.g:361:2: ( ( 'EASY' ) )
                    {
                    // InternalRealmForgeDsl.g:361:2: ( ( 'EASY' ) )
                    // InternalRealmForgeDsl.g:362:3: ( 'EASY' )
                    {
                     before(grammarAccess.getDifficultyAccess().getEASYEnumLiteralDeclaration_0()); 
                    // InternalRealmForgeDsl.g:363:3: ( 'EASY' )
                    // InternalRealmForgeDsl.g:363:4: 'EASY'
                    {
                    match(input,13,FOLLOW_2); 

                    }

                     after(grammarAccess.getDifficultyAccess().getEASYEnumLiteralDeclaration_0()); 

                    }


                    }
                    break;
                case 2 :
                    // InternalRealmForgeDsl.g:367:2: ( ( 'NORMAL' ) )
                    {
                    // InternalRealmForgeDsl.g:367:2: ( ( 'NORMAL' ) )
                    // InternalRealmForgeDsl.g:368:3: ( 'NORMAL' )
                    {
                     before(grammarAccess.getDifficultyAccess().getNORMALEnumLiteralDeclaration_1()); 
                    // InternalRealmForgeDsl.g:369:3: ( 'NORMAL' )
                    // InternalRealmForgeDsl.g:369:4: 'NORMAL'
                    {
                    match(input,14,FOLLOW_2); 

                    }

                     after(grammarAccess.getDifficultyAccess().getNORMALEnumLiteralDeclaration_1()); 

                    }


                    }
                    break;
                case 3 :
                    // InternalRealmForgeDsl.g:373:2: ( ( 'HARD' ) )
                    {
                    // InternalRealmForgeDsl.g:373:2: ( ( 'HARD' ) )
                    // InternalRealmForgeDsl.g:374:3: ( 'HARD' )
                    {
                     before(grammarAccess.getDifficultyAccess().getHARDEnumLiteralDeclaration_2()); 
                    // InternalRealmForgeDsl.g:375:3: ( 'HARD' )
                    // InternalRealmForgeDsl.g:375:4: 'HARD'
                    {
                    match(input,15,FOLLOW_2); 

                    }

                     after(grammarAccess.getDifficultyAccess().getHARDEnumLiteralDeclaration_2()); 

                    }


                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Difficulty__Alternatives"


    // $ANTLR start "rule__EventPack__Group__0"
    // InternalRealmForgeDsl.g:383:1: rule__EventPack__Group__0 : rule__EventPack__Group__0__Impl rule__EventPack__Group__1 ;
    public final void rule__EventPack__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:387:1: ( rule__EventPack__Group__0__Impl rule__EventPack__Group__1 )
            // InternalRealmForgeDsl.g:388:2: rule__EventPack__Group__0__Impl rule__EventPack__Group__1
            {
            pushFollow(FOLLOW_3);
            rule__EventPack__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventPack__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__0"


    // $ANTLR start "rule__EventPack__Group__0__Impl"
    // InternalRealmForgeDsl.g:395:1: rule__EventPack__Group__0__Impl : ( 'name' ) ;
    public final void rule__EventPack__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:399:1: ( ( 'name' ) )
            // InternalRealmForgeDsl.g:400:1: ( 'name' )
            {
            // InternalRealmForgeDsl.g:400:1: ( 'name' )
            // InternalRealmForgeDsl.g:401:2: 'name'
            {
             before(grammarAccess.getEventPackAccess().getNameKeyword_0()); 
            match(input,16,FOLLOW_2); 
             after(grammarAccess.getEventPackAccess().getNameKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__0__Impl"


    // $ANTLR start "rule__EventPack__Group__1"
    // InternalRealmForgeDsl.g:410:1: rule__EventPack__Group__1 : rule__EventPack__Group__1__Impl rule__EventPack__Group__2 ;
    public final void rule__EventPack__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:414:1: ( rule__EventPack__Group__1__Impl rule__EventPack__Group__2 )
            // InternalRealmForgeDsl.g:415:2: rule__EventPack__Group__1__Impl rule__EventPack__Group__2
            {
            pushFollow(FOLLOW_4);
            rule__EventPack__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventPack__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__1"


    // $ANTLR start "rule__EventPack__Group__1__Impl"
    // InternalRealmForgeDsl.g:422:1: rule__EventPack__Group__1__Impl : ( ( rule__EventPack__NameAssignment_1 ) ) ;
    public final void rule__EventPack__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:426:1: ( ( ( rule__EventPack__NameAssignment_1 ) ) )
            // InternalRealmForgeDsl.g:427:1: ( ( rule__EventPack__NameAssignment_1 ) )
            {
            // InternalRealmForgeDsl.g:427:1: ( ( rule__EventPack__NameAssignment_1 ) )
            // InternalRealmForgeDsl.g:428:2: ( rule__EventPack__NameAssignment_1 )
            {
             before(grammarAccess.getEventPackAccess().getNameAssignment_1()); 
            // InternalRealmForgeDsl.g:429:2: ( rule__EventPack__NameAssignment_1 )
            // InternalRealmForgeDsl.g:429:3: rule__EventPack__NameAssignment_1
            {
            pushFollow(FOLLOW_2);
            rule__EventPack__NameAssignment_1();

            state._fsp--;


            }

             after(grammarAccess.getEventPackAccess().getNameAssignment_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__1__Impl"


    // $ANTLR start "rule__EventPack__Group__2"
    // InternalRealmForgeDsl.g:437:1: rule__EventPack__Group__2 : rule__EventPack__Group__2__Impl rule__EventPack__Group__3 ;
    public final void rule__EventPack__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:441:1: ( rule__EventPack__Group__2__Impl rule__EventPack__Group__3 )
            // InternalRealmForgeDsl.g:442:2: rule__EventPack__Group__2__Impl rule__EventPack__Group__3
            {
            pushFollow(FOLLOW_3);
            rule__EventPack__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventPack__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__2"


    // $ANTLR start "rule__EventPack__Group__2__Impl"
    // InternalRealmForgeDsl.g:449:1: rule__EventPack__Group__2__Impl : ( 'description' ) ;
    public final void rule__EventPack__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:453:1: ( ( 'description' ) )
            // InternalRealmForgeDsl.g:454:1: ( 'description' )
            {
            // InternalRealmForgeDsl.g:454:1: ( 'description' )
            // InternalRealmForgeDsl.g:455:2: 'description'
            {
             before(grammarAccess.getEventPackAccess().getDescriptionKeyword_2()); 
            match(input,17,FOLLOW_2); 
             after(grammarAccess.getEventPackAccess().getDescriptionKeyword_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__2__Impl"


    // $ANTLR start "rule__EventPack__Group__3"
    // InternalRealmForgeDsl.g:464:1: rule__EventPack__Group__3 : rule__EventPack__Group__3__Impl rule__EventPack__Group__4 ;
    public final void rule__EventPack__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:468:1: ( rule__EventPack__Group__3__Impl rule__EventPack__Group__4 )
            // InternalRealmForgeDsl.g:469:2: rule__EventPack__Group__3__Impl rule__EventPack__Group__4
            {
            pushFollow(FOLLOW_5);
            rule__EventPack__Group__3__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventPack__Group__4();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__3"


    // $ANTLR start "rule__EventPack__Group__3__Impl"
    // InternalRealmForgeDsl.g:476:1: rule__EventPack__Group__3__Impl : ( ( rule__EventPack__DescriptionAssignment_3 ) ) ;
    public final void rule__EventPack__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:480:1: ( ( ( rule__EventPack__DescriptionAssignment_3 ) ) )
            // InternalRealmForgeDsl.g:481:1: ( ( rule__EventPack__DescriptionAssignment_3 ) )
            {
            // InternalRealmForgeDsl.g:481:1: ( ( rule__EventPack__DescriptionAssignment_3 ) )
            // InternalRealmForgeDsl.g:482:2: ( rule__EventPack__DescriptionAssignment_3 )
            {
             before(grammarAccess.getEventPackAccess().getDescriptionAssignment_3()); 
            // InternalRealmForgeDsl.g:483:2: ( rule__EventPack__DescriptionAssignment_3 )
            // InternalRealmForgeDsl.g:483:3: rule__EventPack__DescriptionAssignment_3
            {
            pushFollow(FOLLOW_2);
            rule__EventPack__DescriptionAssignment_3();

            state._fsp--;


            }

             after(grammarAccess.getEventPackAccess().getDescriptionAssignment_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__3__Impl"


    // $ANTLR start "rule__EventPack__Group__4"
    // InternalRealmForgeDsl.g:491:1: rule__EventPack__Group__4 : rule__EventPack__Group__4__Impl rule__EventPack__Group__5 ;
    public final void rule__EventPack__Group__4() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:495:1: ( rule__EventPack__Group__4__Impl rule__EventPack__Group__5 )
            // InternalRealmForgeDsl.g:496:2: rule__EventPack__Group__4__Impl rule__EventPack__Group__5
            {
            pushFollow(FOLLOW_3);
            rule__EventPack__Group__4__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventPack__Group__5();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__4"


    // $ANTLR start "rule__EventPack__Group__4__Impl"
    // InternalRealmForgeDsl.g:503:1: rule__EventPack__Group__4__Impl : ( 'unit' ) ;
    public final void rule__EventPack__Group__4__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:507:1: ( ( 'unit' ) )
            // InternalRealmForgeDsl.g:508:1: ( 'unit' )
            {
            // InternalRealmForgeDsl.g:508:1: ( 'unit' )
            // InternalRealmForgeDsl.g:509:2: 'unit'
            {
             before(grammarAccess.getEventPackAccess().getUnitKeyword_4()); 
            match(input,18,FOLLOW_2); 
             after(grammarAccess.getEventPackAccess().getUnitKeyword_4()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__4__Impl"


    // $ANTLR start "rule__EventPack__Group__5"
    // InternalRealmForgeDsl.g:518:1: rule__EventPack__Group__5 : rule__EventPack__Group__5__Impl rule__EventPack__Group__6 ;
    public final void rule__EventPack__Group__5() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:522:1: ( rule__EventPack__Group__5__Impl rule__EventPack__Group__6 )
            // InternalRealmForgeDsl.g:523:2: rule__EventPack__Group__5__Impl rule__EventPack__Group__6
            {
            pushFollow(FOLLOW_6);
            rule__EventPack__Group__5__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventPack__Group__6();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__5"


    // $ANTLR start "rule__EventPack__Group__5__Impl"
    // InternalRealmForgeDsl.g:530:1: rule__EventPack__Group__5__Impl : ( ( rule__EventPack__UnitAssignment_5 ) ) ;
    public final void rule__EventPack__Group__5__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:534:1: ( ( ( rule__EventPack__UnitAssignment_5 ) ) )
            // InternalRealmForgeDsl.g:535:1: ( ( rule__EventPack__UnitAssignment_5 ) )
            {
            // InternalRealmForgeDsl.g:535:1: ( ( rule__EventPack__UnitAssignment_5 ) )
            // InternalRealmForgeDsl.g:536:2: ( rule__EventPack__UnitAssignment_5 )
            {
             before(grammarAccess.getEventPackAccess().getUnitAssignment_5()); 
            // InternalRealmForgeDsl.g:537:2: ( rule__EventPack__UnitAssignment_5 )
            // InternalRealmForgeDsl.g:537:3: rule__EventPack__UnitAssignment_5
            {
            pushFollow(FOLLOW_2);
            rule__EventPack__UnitAssignment_5();

            state._fsp--;


            }

             after(grammarAccess.getEventPackAccess().getUnitAssignment_5()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__5__Impl"


    // $ANTLR start "rule__EventPack__Group__6"
    // InternalRealmForgeDsl.g:545:1: rule__EventPack__Group__6 : rule__EventPack__Group__6__Impl ;
    public final void rule__EventPack__Group__6() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:549:1: ( rule__EventPack__Group__6__Impl )
            // InternalRealmForgeDsl.g:550:2: rule__EventPack__Group__6__Impl
            {
            pushFollow(FOLLOW_2);
            rule__EventPack__Group__6__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__6"


    // $ANTLR start "rule__EventPack__Group__6__Impl"
    // InternalRealmForgeDsl.g:556:1: rule__EventPack__Group__6__Impl : ( ( rule__EventPack__EventsAssignment_6 )* ) ;
    public final void rule__EventPack__Group__6__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:560:1: ( ( ( rule__EventPack__EventsAssignment_6 )* ) )
            // InternalRealmForgeDsl.g:561:1: ( ( rule__EventPack__EventsAssignment_6 )* )
            {
            // InternalRealmForgeDsl.g:561:1: ( ( rule__EventPack__EventsAssignment_6 )* )
            // InternalRealmForgeDsl.g:562:2: ( rule__EventPack__EventsAssignment_6 )*
            {
             before(grammarAccess.getEventPackAccess().getEventsAssignment_6()); 
            // InternalRealmForgeDsl.g:563:2: ( rule__EventPack__EventsAssignment_6 )*
            loop5:
            do {
                int alt5=2;
                int LA5_0 = input.LA(1);

                if ( (LA5_0==24||LA5_0==30) ) {
                    alt5=1;
                }


                switch (alt5) {
            	case 1 :
            	    // InternalRealmForgeDsl.g:563:3: rule__EventPack__EventsAssignment_6
            	    {
            	    pushFollow(FOLLOW_7);
            	    rule__EventPack__EventsAssignment_6();

            	    state._fsp--;


            	    }
            	    break;

            	default :
            	    break loop5;
                }
            } while (true);

             after(grammarAccess.getEventPackAccess().getEventsAssignment_6()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__Group__6__Impl"


    // $ANTLR start "rule__EDoubleObject__Group__0"
    // InternalRealmForgeDsl.g:572:1: rule__EDoubleObject__Group__0 : rule__EDoubleObject__Group__0__Impl rule__EDoubleObject__Group__1 ;
    public final void rule__EDoubleObject__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:576:1: ( rule__EDoubleObject__Group__0__Impl rule__EDoubleObject__Group__1 )
            // InternalRealmForgeDsl.g:577:2: rule__EDoubleObject__Group__0__Impl rule__EDoubleObject__Group__1
            {
            pushFollow(FOLLOW_8);
            rule__EDoubleObject__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__0"


    // $ANTLR start "rule__EDoubleObject__Group__0__Impl"
    // InternalRealmForgeDsl.g:584:1: rule__EDoubleObject__Group__0__Impl : ( ( '-' )? ) ;
    public final void rule__EDoubleObject__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:588:1: ( ( ( '-' )? ) )
            // InternalRealmForgeDsl.g:589:1: ( ( '-' )? )
            {
            // InternalRealmForgeDsl.g:589:1: ( ( '-' )? )
            // InternalRealmForgeDsl.g:590:2: ( '-' )?
            {
             before(grammarAccess.getEDoubleObjectAccess().getHyphenMinusKeyword_0()); 
            // InternalRealmForgeDsl.g:591:2: ( '-' )?
            int alt6=2;
            int LA6_0 = input.LA(1);

            if ( (LA6_0==19) ) {
                alt6=1;
            }
            switch (alt6) {
                case 1 :
                    // InternalRealmForgeDsl.g:591:3: '-'
                    {
                    match(input,19,FOLLOW_2); 

                    }
                    break;

            }

             after(grammarAccess.getEDoubleObjectAccess().getHyphenMinusKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__0__Impl"


    // $ANTLR start "rule__EDoubleObject__Group__1"
    // InternalRealmForgeDsl.g:599:1: rule__EDoubleObject__Group__1 : rule__EDoubleObject__Group__1__Impl rule__EDoubleObject__Group__2 ;
    public final void rule__EDoubleObject__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:603:1: ( rule__EDoubleObject__Group__1__Impl rule__EDoubleObject__Group__2 )
            // InternalRealmForgeDsl.g:604:2: rule__EDoubleObject__Group__1__Impl rule__EDoubleObject__Group__2
            {
            pushFollow(FOLLOW_8);
            rule__EDoubleObject__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__1"


    // $ANTLR start "rule__EDoubleObject__Group__1__Impl"
    // InternalRealmForgeDsl.g:611:1: rule__EDoubleObject__Group__1__Impl : ( ( RULE_INT )? ) ;
    public final void rule__EDoubleObject__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:615:1: ( ( ( RULE_INT )? ) )
            // InternalRealmForgeDsl.g:616:1: ( ( RULE_INT )? )
            {
            // InternalRealmForgeDsl.g:616:1: ( ( RULE_INT )? )
            // InternalRealmForgeDsl.g:617:2: ( RULE_INT )?
            {
             before(grammarAccess.getEDoubleObjectAccess().getINTTerminalRuleCall_1()); 
            // InternalRealmForgeDsl.g:618:2: ( RULE_INT )?
            int alt7=2;
            int LA7_0 = input.LA(1);

            if ( (LA7_0==RULE_INT) ) {
                alt7=1;
            }
            switch (alt7) {
                case 1 :
                    // InternalRealmForgeDsl.g:618:3: RULE_INT
                    {
                    match(input,RULE_INT,FOLLOW_2); 

                    }
                    break;

            }

             after(grammarAccess.getEDoubleObjectAccess().getINTTerminalRuleCall_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__1__Impl"


    // $ANTLR start "rule__EDoubleObject__Group__2"
    // InternalRealmForgeDsl.g:626:1: rule__EDoubleObject__Group__2 : rule__EDoubleObject__Group__2__Impl rule__EDoubleObject__Group__3 ;
    public final void rule__EDoubleObject__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:630:1: ( rule__EDoubleObject__Group__2__Impl rule__EDoubleObject__Group__3 )
            // InternalRealmForgeDsl.g:631:2: rule__EDoubleObject__Group__2__Impl rule__EDoubleObject__Group__3
            {
            pushFollow(FOLLOW_9);
            rule__EDoubleObject__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__2"


    // $ANTLR start "rule__EDoubleObject__Group__2__Impl"
    // InternalRealmForgeDsl.g:638:1: rule__EDoubleObject__Group__2__Impl : ( '.' ) ;
    public final void rule__EDoubleObject__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:642:1: ( ( '.' ) )
            // InternalRealmForgeDsl.g:643:1: ( '.' )
            {
            // InternalRealmForgeDsl.g:643:1: ( '.' )
            // InternalRealmForgeDsl.g:644:2: '.'
            {
             before(grammarAccess.getEDoubleObjectAccess().getFullStopKeyword_2()); 
            match(input,20,FOLLOW_2); 
             after(grammarAccess.getEDoubleObjectAccess().getFullStopKeyword_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__2__Impl"


    // $ANTLR start "rule__EDoubleObject__Group__3"
    // InternalRealmForgeDsl.g:653:1: rule__EDoubleObject__Group__3 : rule__EDoubleObject__Group__3__Impl rule__EDoubleObject__Group__4 ;
    public final void rule__EDoubleObject__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:657:1: ( rule__EDoubleObject__Group__3__Impl rule__EDoubleObject__Group__4 )
            // InternalRealmForgeDsl.g:658:2: rule__EDoubleObject__Group__3__Impl rule__EDoubleObject__Group__4
            {
            pushFollow(FOLLOW_10);
            rule__EDoubleObject__Group__3__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Group__4();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__3"


    // $ANTLR start "rule__EDoubleObject__Group__3__Impl"
    // InternalRealmForgeDsl.g:665:1: rule__EDoubleObject__Group__3__Impl : ( RULE_INT ) ;
    public final void rule__EDoubleObject__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:669:1: ( ( RULE_INT ) )
            // InternalRealmForgeDsl.g:670:1: ( RULE_INT )
            {
            // InternalRealmForgeDsl.g:670:1: ( RULE_INT )
            // InternalRealmForgeDsl.g:671:2: RULE_INT
            {
             before(grammarAccess.getEDoubleObjectAccess().getINTTerminalRuleCall_3()); 
            match(input,RULE_INT,FOLLOW_2); 
             after(grammarAccess.getEDoubleObjectAccess().getINTTerminalRuleCall_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__3__Impl"


    // $ANTLR start "rule__EDoubleObject__Group__4"
    // InternalRealmForgeDsl.g:680:1: rule__EDoubleObject__Group__4 : rule__EDoubleObject__Group__4__Impl ;
    public final void rule__EDoubleObject__Group__4() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:684:1: ( rule__EDoubleObject__Group__4__Impl )
            // InternalRealmForgeDsl.g:685:2: rule__EDoubleObject__Group__4__Impl
            {
            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Group__4__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__4"


    // $ANTLR start "rule__EDoubleObject__Group__4__Impl"
    // InternalRealmForgeDsl.g:691:1: rule__EDoubleObject__Group__4__Impl : ( ( rule__EDoubleObject__Group_4__0 )? ) ;
    public final void rule__EDoubleObject__Group__4__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:695:1: ( ( ( rule__EDoubleObject__Group_4__0 )? ) )
            // InternalRealmForgeDsl.g:696:1: ( ( rule__EDoubleObject__Group_4__0 )? )
            {
            // InternalRealmForgeDsl.g:696:1: ( ( rule__EDoubleObject__Group_4__0 )? )
            // InternalRealmForgeDsl.g:697:2: ( rule__EDoubleObject__Group_4__0 )?
            {
             before(grammarAccess.getEDoubleObjectAccess().getGroup_4()); 
            // InternalRealmForgeDsl.g:698:2: ( rule__EDoubleObject__Group_4__0 )?
            int alt8=2;
            int LA8_0 = input.LA(1);

            if ( ((LA8_0>=11 && LA8_0<=12)) ) {
                alt8=1;
            }
            switch (alt8) {
                case 1 :
                    // InternalRealmForgeDsl.g:698:3: rule__EDoubleObject__Group_4__0
                    {
                    pushFollow(FOLLOW_2);
                    rule__EDoubleObject__Group_4__0();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getEDoubleObjectAccess().getGroup_4()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group__4__Impl"


    // $ANTLR start "rule__EDoubleObject__Group_4__0"
    // InternalRealmForgeDsl.g:707:1: rule__EDoubleObject__Group_4__0 : rule__EDoubleObject__Group_4__0__Impl rule__EDoubleObject__Group_4__1 ;
    public final void rule__EDoubleObject__Group_4__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:711:1: ( rule__EDoubleObject__Group_4__0__Impl rule__EDoubleObject__Group_4__1 )
            // InternalRealmForgeDsl.g:712:2: rule__EDoubleObject__Group_4__0__Impl rule__EDoubleObject__Group_4__1
            {
            pushFollow(FOLLOW_11);
            rule__EDoubleObject__Group_4__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Group_4__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group_4__0"


    // $ANTLR start "rule__EDoubleObject__Group_4__0__Impl"
    // InternalRealmForgeDsl.g:719:1: rule__EDoubleObject__Group_4__0__Impl : ( ( rule__EDoubleObject__Alternatives_4_0 ) ) ;
    public final void rule__EDoubleObject__Group_4__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:723:1: ( ( ( rule__EDoubleObject__Alternatives_4_0 ) ) )
            // InternalRealmForgeDsl.g:724:1: ( ( rule__EDoubleObject__Alternatives_4_0 ) )
            {
            // InternalRealmForgeDsl.g:724:1: ( ( rule__EDoubleObject__Alternatives_4_0 ) )
            // InternalRealmForgeDsl.g:725:2: ( rule__EDoubleObject__Alternatives_4_0 )
            {
             before(grammarAccess.getEDoubleObjectAccess().getAlternatives_4_0()); 
            // InternalRealmForgeDsl.g:726:2: ( rule__EDoubleObject__Alternatives_4_0 )
            // InternalRealmForgeDsl.g:726:3: rule__EDoubleObject__Alternatives_4_0
            {
            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Alternatives_4_0();

            state._fsp--;


            }

             after(grammarAccess.getEDoubleObjectAccess().getAlternatives_4_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group_4__0__Impl"


    // $ANTLR start "rule__EDoubleObject__Group_4__1"
    // InternalRealmForgeDsl.g:734:1: rule__EDoubleObject__Group_4__1 : rule__EDoubleObject__Group_4__1__Impl rule__EDoubleObject__Group_4__2 ;
    public final void rule__EDoubleObject__Group_4__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:738:1: ( rule__EDoubleObject__Group_4__1__Impl rule__EDoubleObject__Group_4__2 )
            // InternalRealmForgeDsl.g:739:2: rule__EDoubleObject__Group_4__1__Impl rule__EDoubleObject__Group_4__2
            {
            pushFollow(FOLLOW_11);
            rule__EDoubleObject__Group_4__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Group_4__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group_4__1"


    // $ANTLR start "rule__EDoubleObject__Group_4__1__Impl"
    // InternalRealmForgeDsl.g:746:1: rule__EDoubleObject__Group_4__1__Impl : ( ( '-' )? ) ;
    public final void rule__EDoubleObject__Group_4__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:750:1: ( ( ( '-' )? ) )
            // InternalRealmForgeDsl.g:751:1: ( ( '-' )? )
            {
            // InternalRealmForgeDsl.g:751:1: ( ( '-' )? )
            // InternalRealmForgeDsl.g:752:2: ( '-' )?
            {
             before(grammarAccess.getEDoubleObjectAccess().getHyphenMinusKeyword_4_1()); 
            // InternalRealmForgeDsl.g:753:2: ( '-' )?
            int alt9=2;
            int LA9_0 = input.LA(1);

            if ( (LA9_0==19) ) {
                alt9=1;
            }
            switch (alt9) {
                case 1 :
                    // InternalRealmForgeDsl.g:753:3: '-'
                    {
                    match(input,19,FOLLOW_2); 

                    }
                    break;

            }

             after(grammarAccess.getEDoubleObjectAccess().getHyphenMinusKeyword_4_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group_4__1__Impl"


    // $ANTLR start "rule__EDoubleObject__Group_4__2"
    // InternalRealmForgeDsl.g:761:1: rule__EDoubleObject__Group_4__2 : rule__EDoubleObject__Group_4__2__Impl ;
    public final void rule__EDoubleObject__Group_4__2() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:765:1: ( rule__EDoubleObject__Group_4__2__Impl )
            // InternalRealmForgeDsl.g:766:2: rule__EDoubleObject__Group_4__2__Impl
            {
            pushFollow(FOLLOW_2);
            rule__EDoubleObject__Group_4__2__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group_4__2"


    // $ANTLR start "rule__EDoubleObject__Group_4__2__Impl"
    // InternalRealmForgeDsl.g:772:1: rule__EDoubleObject__Group_4__2__Impl : ( RULE_INT ) ;
    public final void rule__EDoubleObject__Group_4__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:776:1: ( ( RULE_INT ) )
            // InternalRealmForgeDsl.g:777:1: ( RULE_INT )
            {
            // InternalRealmForgeDsl.g:777:1: ( RULE_INT )
            // InternalRealmForgeDsl.g:778:2: RULE_INT
            {
             before(grammarAccess.getEDoubleObjectAccess().getINTTerminalRuleCall_4_2()); 
            match(input,RULE_INT,FOLLOW_2); 
             after(grammarAccess.getEDoubleObjectAccess().getINTTerminalRuleCall_4_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EDoubleObject__Group_4__2__Impl"


    // $ANTLR start "rule__EInt__Group__0"
    // InternalRealmForgeDsl.g:788:1: rule__EInt__Group__0 : rule__EInt__Group__0__Impl rule__EInt__Group__1 ;
    public final void rule__EInt__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:792:1: ( rule__EInt__Group__0__Impl rule__EInt__Group__1 )
            // InternalRealmForgeDsl.g:793:2: rule__EInt__Group__0__Impl rule__EInt__Group__1
            {
            pushFollow(FOLLOW_9);
            rule__EInt__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EInt__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EInt__Group__0"


    // $ANTLR start "rule__EInt__Group__0__Impl"
    // InternalRealmForgeDsl.g:800:1: rule__EInt__Group__0__Impl : ( ( '-' )? ) ;
    public final void rule__EInt__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:804:1: ( ( ( '-' )? ) )
            // InternalRealmForgeDsl.g:805:1: ( ( '-' )? )
            {
            // InternalRealmForgeDsl.g:805:1: ( ( '-' )? )
            // InternalRealmForgeDsl.g:806:2: ( '-' )?
            {
             before(grammarAccess.getEIntAccess().getHyphenMinusKeyword_0()); 
            // InternalRealmForgeDsl.g:807:2: ( '-' )?
            int alt10=2;
            int LA10_0 = input.LA(1);

            if ( (LA10_0==19) ) {
                alt10=1;
            }
            switch (alt10) {
                case 1 :
                    // InternalRealmForgeDsl.g:807:3: '-'
                    {
                    match(input,19,FOLLOW_2); 

                    }
                    break;

            }

             after(grammarAccess.getEIntAccess().getHyphenMinusKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EInt__Group__0__Impl"


    // $ANTLR start "rule__EInt__Group__1"
    // InternalRealmForgeDsl.g:815:1: rule__EInt__Group__1 : rule__EInt__Group__1__Impl ;
    public final void rule__EInt__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:819:1: ( rule__EInt__Group__1__Impl )
            // InternalRealmForgeDsl.g:820:2: rule__EInt__Group__1__Impl
            {
            pushFollow(FOLLOW_2);
            rule__EInt__Group__1__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EInt__Group__1"


    // $ANTLR start "rule__EInt__Group__1__Impl"
    // InternalRealmForgeDsl.g:826:1: rule__EInt__Group__1__Impl : ( RULE_INT ) ;
    public final void rule__EInt__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:830:1: ( ( RULE_INT ) )
            // InternalRealmForgeDsl.g:831:1: ( RULE_INT )
            {
            // InternalRealmForgeDsl.g:831:1: ( RULE_INT )
            // InternalRealmForgeDsl.g:832:2: RULE_INT
            {
             before(grammarAccess.getEIntAccess().getINTTerminalRuleCall_1()); 
            match(input,RULE_INT,FOLLOW_2); 
             after(grammarAccess.getEIntAccess().getINTTerminalRuleCall_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EInt__Group__1__Impl"


    // $ANTLR start "rule__EventResult__Group__0"
    // InternalRealmForgeDsl.g:842:1: rule__EventResult__Group__0 : rule__EventResult__Group__0__Impl rule__EventResult__Group__1 ;
    public final void rule__EventResult__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:846:1: ( rule__EventResult__Group__0__Impl rule__EventResult__Group__1 )
            // InternalRealmForgeDsl.g:847:2: rule__EventResult__Group__0__Impl rule__EventResult__Group__1
            {
            pushFollow(FOLLOW_12);
            rule__EventResult__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventResult__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group__0"


    // $ANTLR start "rule__EventResult__Group__0__Impl"
    // InternalRealmForgeDsl.g:854:1: rule__EventResult__Group__0__Impl : ( () ) ;
    public final void rule__EventResult__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:858:1: ( ( () ) )
            // InternalRealmForgeDsl.g:859:1: ( () )
            {
            // InternalRealmForgeDsl.g:859:1: ( () )
            // InternalRealmForgeDsl.g:860:2: ()
            {
             before(grammarAccess.getEventResultAccess().getEventResultAction_0()); 
            // InternalRealmForgeDsl.g:861:2: ()
            // InternalRealmForgeDsl.g:861:3: 
            {
            }

             after(grammarAccess.getEventResultAccess().getEventResultAction_0()); 

            }


            }

        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group__0__Impl"


    // $ANTLR start "rule__EventResult__Group__1"
    // InternalRealmForgeDsl.g:869:1: rule__EventResult__Group__1 : rule__EventResult__Group__1__Impl rule__EventResult__Group__2 ;
    public final void rule__EventResult__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:873:1: ( rule__EventResult__Group__1__Impl rule__EventResult__Group__2 )
            // InternalRealmForgeDsl.g:874:2: rule__EventResult__Group__1__Impl rule__EventResult__Group__2
            {
            pushFollow(FOLLOW_13);
            rule__EventResult__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventResult__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group__1"


    // $ANTLR start "rule__EventResult__Group__1__Impl"
    // InternalRealmForgeDsl.g:881:1: rule__EventResult__Group__1__Impl : ( '{' ) ;
    public final void rule__EventResult__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:885:1: ( ( '{' ) )
            // InternalRealmForgeDsl.g:886:1: ( '{' )
            {
            // InternalRealmForgeDsl.g:886:1: ( '{' )
            // InternalRealmForgeDsl.g:887:2: '{'
            {
             before(grammarAccess.getEventResultAccess().getLeftCurlyBracketKeyword_1()); 
            match(input,21,FOLLOW_2); 
             after(grammarAccess.getEventResultAccess().getLeftCurlyBracketKeyword_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group__1__Impl"


    // $ANTLR start "rule__EventResult__Group__2"
    // InternalRealmForgeDsl.g:896:1: rule__EventResult__Group__2 : rule__EventResult__Group__2__Impl rule__EventResult__Group__3 ;
    public final void rule__EventResult__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:900:1: ( rule__EventResult__Group__2__Impl rule__EventResult__Group__3 )
            // InternalRealmForgeDsl.g:901:2: rule__EventResult__Group__2__Impl rule__EventResult__Group__3
            {
            pushFollow(FOLLOW_13);
            rule__EventResult__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventResult__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group__2"


    // $ANTLR start "rule__EventResult__Group__2__Impl"
    // InternalRealmForgeDsl.g:908:1: rule__EventResult__Group__2__Impl : ( ( rule__EventResult__Group_2__0 )? ) ;
    public final void rule__EventResult__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:912:1: ( ( ( rule__EventResult__Group_2__0 )? ) )
            // InternalRealmForgeDsl.g:913:1: ( ( rule__EventResult__Group_2__0 )? )
            {
            // InternalRealmForgeDsl.g:913:1: ( ( rule__EventResult__Group_2__0 )? )
            // InternalRealmForgeDsl.g:914:2: ( rule__EventResult__Group_2__0 )?
            {
             before(grammarAccess.getEventResultAccess().getGroup_2()); 
            // InternalRealmForgeDsl.g:915:2: ( rule__EventResult__Group_2__0 )?
            int alt11=2;
            int LA11_0 = input.LA(1);

            if ( (LA11_0==23) ) {
                alt11=1;
            }
            switch (alt11) {
                case 1 :
                    // InternalRealmForgeDsl.g:915:3: rule__EventResult__Group_2__0
                    {
                    pushFollow(FOLLOW_2);
                    rule__EventResult__Group_2__0();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getEventResultAccess().getGroup_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group__2__Impl"


    // $ANTLR start "rule__EventResult__Group__3"
    // InternalRealmForgeDsl.g:923:1: rule__EventResult__Group__3 : rule__EventResult__Group__3__Impl ;
    public final void rule__EventResult__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:927:1: ( rule__EventResult__Group__3__Impl )
            // InternalRealmForgeDsl.g:928:2: rule__EventResult__Group__3__Impl
            {
            pushFollow(FOLLOW_2);
            rule__EventResult__Group__3__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group__3"


    // $ANTLR start "rule__EventResult__Group__3__Impl"
    // InternalRealmForgeDsl.g:934:1: rule__EventResult__Group__3__Impl : ( '}' ) ;
    public final void rule__EventResult__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:938:1: ( ( '}' ) )
            // InternalRealmForgeDsl.g:939:1: ( '}' )
            {
            // InternalRealmForgeDsl.g:939:1: ( '}' )
            // InternalRealmForgeDsl.g:940:2: '}'
            {
             before(grammarAccess.getEventResultAccess().getRightCurlyBracketKeyword_3()); 
            match(input,22,FOLLOW_2); 
             after(grammarAccess.getEventResultAccess().getRightCurlyBracketKeyword_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group__3__Impl"


    // $ANTLR start "rule__EventResult__Group_2__0"
    // InternalRealmForgeDsl.g:950:1: rule__EventResult__Group_2__0 : rule__EventResult__Group_2__0__Impl rule__EventResult__Group_2__1 ;
    public final void rule__EventResult__Group_2__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:954:1: ( rule__EventResult__Group_2__0__Impl rule__EventResult__Group_2__1 )
            // InternalRealmForgeDsl.g:955:2: rule__EventResult__Group_2__0__Impl rule__EventResult__Group_2__1
            {
            pushFollow(FOLLOW_3);
            rule__EventResult__Group_2__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__EventResult__Group_2__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group_2__0"


    // $ANTLR start "rule__EventResult__Group_2__0__Impl"
    // InternalRealmForgeDsl.g:962:1: rule__EventResult__Group_2__0__Impl : ( 'message' ) ;
    public final void rule__EventResult__Group_2__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:966:1: ( ( 'message' ) )
            // InternalRealmForgeDsl.g:967:1: ( 'message' )
            {
            // InternalRealmForgeDsl.g:967:1: ( 'message' )
            // InternalRealmForgeDsl.g:968:2: 'message'
            {
             before(grammarAccess.getEventResultAccess().getMessageKeyword_2_0()); 
            match(input,23,FOLLOW_2); 
             after(grammarAccess.getEventResultAccess().getMessageKeyword_2_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group_2__0__Impl"


    // $ANTLR start "rule__EventResult__Group_2__1"
    // InternalRealmForgeDsl.g:977:1: rule__EventResult__Group_2__1 : rule__EventResult__Group_2__1__Impl ;
    public final void rule__EventResult__Group_2__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:981:1: ( rule__EventResult__Group_2__1__Impl )
            // InternalRealmForgeDsl.g:982:2: rule__EventResult__Group_2__1__Impl
            {
            pushFollow(FOLLOW_2);
            rule__EventResult__Group_2__1__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group_2__1"


    // $ANTLR start "rule__EventResult__Group_2__1__Impl"
    // InternalRealmForgeDsl.g:988:1: rule__EventResult__Group_2__1__Impl : ( ( rule__EventResult__MessageAssignment_2_1 ) ) ;
    public final void rule__EventResult__Group_2__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:992:1: ( ( ( rule__EventResult__MessageAssignment_2_1 ) ) )
            // InternalRealmForgeDsl.g:993:1: ( ( rule__EventResult__MessageAssignment_2_1 ) )
            {
            // InternalRealmForgeDsl.g:993:1: ( ( rule__EventResult__MessageAssignment_2_1 ) )
            // InternalRealmForgeDsl.g:994:2: ( rule__EventResult__MessageAssignment_2_1 )
            {
             before(grammarAccess.getEventResultAccess().getMessageAssignment_2_1()); 
            // InternalRealmForgeDsl.g:995:2: ( rule__EventResult__MessageAssignment_2_1 )
            // InternalRealmForgeDsl.g:995:3: rule__EventResult__MessageAssignment_2_1
            {
            pushFollow(FOLLOW_2);
            rule__EventResult__MessageAssignment_2_1();

            state._fsp--;


            }

             after(grammarAccess.getEventResultAccess().getMessageAssignment_2_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__Group_2__1__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__0"
    // InternalRealmForgeDsl.g:1004:1: rule__TypeRacerEvent__Group__0 : rule__TypeRacerEvent__Group__0__Impl rule__TypeRacerEvent__Group__1 ;
    public final void rule__TypeRacerEvent__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1008:1: ( rule__TypeRacerEvent__Group__0__Impl rule__TypeRacerEvent__Group__1 )
            // InternalRealmForgeDsl.g:1009:2: rule__TypeRacerEvent__Group__0__Impl rule__TypeRacerEvent__Group__1
            {
            pushFollow(FOLLOW_12);
            rule__TypeRacerEvent__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__0"


    // $ANTLR start "rule__TypeRacerEvent__Group__0__Impl"
    // InternalRealmForgeDsl.g:1016:1: rule__TypeRacerEvent__Group__0__Impl : ( 'TypeRacerEvent' ) ;
    public final void rule__TypeRacerEvent__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1020:1: ( ( 'TypeRacerEvent' ) )
            // InternalRealmForgeDsl.g:1021:1: ( 'TypeRacerEvent' )
            {
            // InternalRealmForgeDsl.g:1021:1: ( 'TypeRacerEvent' )
            // InternalRealmForgeDsl.g:1022:2: 'TypeRacerEvent'
            {
             before(grammarAccess.getTypeRacerEventAccess().getTypeRacerEventKeyword_0()); 
            match(input,24,FOLLOW_2); 
             after(grammarAccess.getTypeRacerEventAccess().getTypeRacerEventKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__0__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__1"
    // InternalRealmForgeDsl.g:1031:1: rule__TypeRacerEvent__Group__1 : rule__TypeRacerEvent__Group__1__Impl rule__TypeRacerEvent__Group__2 ;
    public final void rule__TypeRacerEvent__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1035:1: ( rule__TypeRacerEvent__Group__1__Impl rule__TypeRacerEvent__Group__2 )
            // InternalRealmForgeDsl.g:1036:2: rule__TypeRacerEvent__Group__1__Impl rule__TypeRacerEvent__Group__2
            {
            pushFollow(FOLLOW_14);
            rule__TypeRacerEvent__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__1"


    // $ANTLR start "rule__TypeRacerEvent__Group__1__Impl"
    // InternalRealmForgeDsl.g:1043:1: rule__TypeRacerEvent__Group__1__Impl : ( '{' ) ;
    public final void rule__TypeRacerEvent__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1047:1: ( ( '{' ) )
            // InternalRealmForgeDsl.g:1048:1: ( '{' )
            {
            // InternalRealmForgeDsl.g:1048:1: ( '{' )
            // InternalRealmForgeDsl.g:1049:2: '{'
            {
             before(grammarAccess.getTypeRacerEventAccess().getLeftCurlyBracketKeyword_1()); 
            match(input,21,FOLLOW_2); 
             after(grammarAccess.getTypeRacerEventAccess().getLeftCurlyBracketKeyword_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__1__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__2"
    // InternalRealmForgeDsl.g:1058:1: rule__TypeRacerEvent__Group__2 : rule__TypeRacerEvent__Group__2__Impl rule__TypeRacerEvent__Group__3 ;
    public final void rule__TypeRacerEvent__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1062:1: ( rule__TypeRacerEvent__Group__2__Impl rule__TypeRacerEvent__Group__3 )
            // InternalRealmForgeDsl.g:1063:2: rule__TypeRacerEvent__Group__2__Impl rule__TypeRacerEvent__Group__3
            {
            pushFollow(FOLLOW_14);
            rule__TypeRacerEvent__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__2"


    // $ANTLR start "rule__TypeRacerEvent__Group__2__Impl"
    // InternalRealmForgeDsl.g:1070:1: rule__TypeRacerEvent__Group__2__Impl : ( ( rule__TypeRacerEvent__IsCaseSensitiveAssignment_2 )? ) ;
    public final void rule__TypeRacerEvent__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1074:1: ( ( ( rule__TypeRacerEvent__IsCaseSensitiveAssignment_2 )? ) )
            // InternalRealmForgeDsl.g:1075:1: ( ( rule__TypeRacerEvent__IsCaseSensitiveAssignment_2 )? )
            {
            // InternalRealmForgeDsl.g:1075:1: ( ( rule__TypeRacerEvent__IsCaseSensitiveAssignment_2 )? )
            // InternalRealmForgeDsl.g:1076:2: ( rule__TypeRacerEvent__IsCaseSensitiveAssignment_2 )?
            {
             before(grammarAccess.getTypeRacerEventAccess().getIsCaseSensitiveAssignment_2()); 
            // InternalRealmForgeDsl.g:1077:2: ( rule__TypeRacerEvent__IsCaseSensitiveAssignment_2 )?
            int alt12=2;
            int LA12_0 = input.LA(1);

            if ( (LA12_0==35) ) {
                alt12=1;
            }
            switch (alt12) {
                case 1 :
                    // InternalRealmForgeDsl.g:1077:3: rule__TypeRacerEvent__IsCaseSensitiveAssignment_2
                    {
                    pushFollow(FOLLOW_2);
                    rule__TypeRacerEvent__IsCaseSensitiveAssignment_2();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getTypeRacerEventAccess().getIsCaseSensitiveAssignment_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__2__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__3"
    // InternalRealmForgeDsl.g:1085:1: rule__TypeRacerEvent__Group__3 : rule__TypeRacerEvent__Group__3__Impl rule__TypeRacerEvent__Group__4 ;
    public final void rule__TypeRacerEvent__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1089:1: ( rule__TypeRacerEvent__Group__3__Impl rule__TypeRacerEvent__Group__4 )
            // InternalRealmForgeDsl.g:1090:2: rule__TypeRacerEvent__Group__3__Impl rule__TypeRacerEvent__Group__4
            {
            pushFollow(FOLLOW_15);
            rule__TypeRacerEvent__Group__3__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__4();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__3"


    // $ANTLR start "rule__TypeRacerEvent__Group__3__Impl"
    // InternalRealmForgeDsl.g:1097:1: rule__TypeRacerEvent__Group__3__Impl : ( 'difficulty' ) ;
    public final void rule__TypeRacerEvent__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1101:1: ( ( 'difficulty' ) )
            // InternalRealmForgeDsl.g:1102:1: ( 'difficulty' )
            {
            // InternalRealmForgeDsl.g:1102:1: ( 'difficulty' )
            // InternalRealmForgeDsl.g:1103:2: 'difficulty'
            {
             before(grammarAccess.getTypeRacerEventAccess().getDifficultyKeyword_3()); 
            match(input,25,FOLLOW_2); 
             after(grammarAccess.getTypeRacerEventAccess().getDifficultyKeyword_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__3__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__4"
    // InternalRealmForgeDsl.g:1112:1: rule__TypeRacerEvent__Group__4 : rule__TypeRacerEvent__Group__4__Impl rule__TypeRacerEvent__Group__5 ;
    public final void rule__TypeRacerEvent__Group__4() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1116:1: ( rule__TypeRacerEvent__Group__4__Impl rule__TypeRacerEvent__Group__5 )
            // InternalRealmForgeDsl.g:1117:2: rule__TypeRacerEvent__Group__4__Impl rule__TypeRacerEvent__Group__5
            {
            pushFollow(FOLLOW_16);
            rule__TypeRacerEvent__Group__4__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__5();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__4"


    // $ANTLR start "rule__TypeRacerEvent__Group__4__Impl"
    // InternalRealmForgeDsl.g:1124:1: rule__TypeRacerEvent__Group__4__Impl : ( ( rule__TypeRacerEvent__DifficultyAssignment_4 ) ) ;
    public final void rule__TypeRacerEvent__Group__4__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1128:1: ( ( ( rule__TypeRacerEvent__DifficultyAssignment_4 ) ) )
            // InternalRealmForgeDsl.g:1129:1: ( ( rule__TypeRacerEvent__DifficultyAssignment_4 ) )
            {
            // InternalRealmForgeDsl.g:1129:1: ( ( rule__TypeRacerEvent__DifficultyAssignment_4 ) )
            // InternalRealmForgeDsl.g:1130:2: ( rule__TypeRacerEvent__DifficultyAssignment_4 )
            {
             before(grammarAccess.getTypeRacerEventAccess().getDifficultyAssignment_4()); 
            // InternalRealmForgeDsl.g:1131:2: ( rule__TypeRacerEvent__DifficultyAssignment_4 )
            // InternalRealmForgeDsl.g:1131:3: rule__TypeRacerEvent__DifficultyAssignment_4
            {
            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__DifficultyAssignment_4();

            state._fsp--;


            }

             after(grammarAccess.getTypeRacerEventAccess().getDifficultyAssignment_4()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__4__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__5"
    // InternalRealmForgeDsl.g:1139:1: rule__TypeRacerEvent__Group__5 : rule__TypeRacerEvent__Group__5__Impl rule__TypeRacerEvent__Group__6 ;
    public final void rule__TypeRacerEvent__Group__5() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1143:1: ( rule__TypeRacerEvent__Group__5__Impl rule__TypeRacerEvent__Group__6 )
            // InternalRealmForgeDsl.g:1144:2: rule__TypeRacerEvent__Group__5__Impl rule__TypeRacerEvent__Group__6
            {
            pushFollow(FOLLOW_16);
            rule__TypeRacerEvent__Group__5__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__6();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__5"


    // $ANTLR start "rule__TypeRacerEvent__Group__5__Impl"
    // InternalRealmForgeDsl.g:1151:1: rule__TypeRacerEvent__Group__5__Impl : ( ( rule__TypeRacerEvent__Group_5__0 )? ) ;
    public final void rule__TypeRacerEvent__Group__5__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1155:1: ( ( ( rule__TypeRacerEvent__Group_5__0 )? ) )
            // InternalRealmForgeDsl.g:1156:1: ( ( rule__TypeRacerEvent__Group_5__0 )? )
            {
            // InternalRealmForgeDsl.g:1156:1: ( ( rule__TypeRacerEvent__Group_5__0 )? )
            // InternalRealmForgeDsl.g:1157:2: ( rule__TypeRacerEvent__Group_5__0 )?
            {
             before(grammarAccess.getTypeRacerEventAccess().getGroup_5()); 
            // InternalRealmForgeDsl.g:1158:2: ( rule__TypeRacerEvent__Group_5__0 )?
            int alt13=2;
            int LA13_0 = input.LA(1);

            if ( (LA13_0==29) ) {
                alt13=1;
            }
            switch (alt13) {
                case 1 :
                    // InternalRealmForgeDsl.g:1158:3: rule__TypeRacerEvent__Group_5__0
                    {
                    pushFollow(FOLLOW_2);
                    rule__TypeRacerEvent__Group_5__0();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getTypeRacerEventAccess().getGroup_5()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__5__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__6"
    // InternalRealmForgeDsl.g:1166:1: rule__TypeRacerEvent__Group__6 : rule__TypeRacerEvent__Group__6__Impl rule__TypeRacerEvent__Group__7 ;
    public final void rule__TypeRacerEvent__Group__6() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1170:1: ( rule__TypeRacerEvent__Group__6__Impl rule__TypeRacerEvent__Group__7 )
            // InternalRealmForgeDsl.g:1171:2: rule__TypeRacerEvent__Group__6__Impl rule__TypeRacerEvent__Group__7
            {
            pushFollow(FOLLOW_11);
            rule__TypeRacerEvent__Group__6__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__7();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__6"


    // $ANTLR start "rule__TypeRacerEvent__Group__6__Impl"
    // InternalRealmForgeDsl.g:1178:1: rule__TypeRacerEvent__Group__6__Impl : ( 'retries' ) ;
    public final void rule__TypeRacerEvent__Group__6__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1182:1: ( ( 'retries' ) )
            // InternalRealmForgeDsl.g:1183:1: ( 'retries' )
            {
            // InternalRealmForgeDsl.g:1183:1: ( 'retries' )
            // InternalRealmForgeDsl.g:1184:2: 'retries'
            {
             before(grammarAccess.getTypeRacerEventAccess().getRetriesKeyword_6()); 
            match(input,26,FOLLOW_2); 
             after(grammarAccess.getTypeRacerEventAccess().getRetriesKeyword_6()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__6__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__7"
    // InternalRealmForgeDsl.g:1193:1: rule__TypeRacerEvent__Group__7 : rule__TypeRacerEvent__Group__7__Impl rule__TypeRacerEvent__Group__8 ;
    public final void rule__TypeRacerEvent__Group__7() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1197:1: ( rule__TypeRacerEvent__Group__7__Impl rule__TypeRacerEvent__Group__8 )
            // InternalRealmForgeDsl.g:1198:2: rule__TypeRacerEvent__Group__7__Impl rule__TypeRacerEvent__Group__8
            {
            pushFollow(FOLLOW_17);
            rule__TypeRacerEvent__Group__7__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__8();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__7"


    // $ANTLR start "rule__TypeRacerEvent__Group__7__Impl"
    // InternalRealmForgeDsl.g:1205:1: rule__TypeRacerEvent__Group__7__Impl : ( ( rule__TypeRacerEvent__RetriesAssignment_7 ) ) ;
    public final void rule__TypeRacerEvent__Group__7__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1209:1: ( ( ( rule__TypeRacerEvent__RetriesAssignment_7 ) ) )
            // InternalRealmForgeDsl.g:1210:1: ( ( rule__TypeRacerEvent__RetriesAssignment_7 ) )
            {
            // InternalRealmForgeDsl.g:1210:1: ( ( rule__TypeRacerEvent__RetriesAssignment_7 ) )
            // InternalRealmForgeDsl.g:1211:2: ( rule__TypeRacerEvent__RetriesAssignment_7 )
            {
             before(grammarAccess.getTypeRacerEventAccess().getRetriesAssignment_7()); 
            // InternalRealmForgeDsl.g:1212:2: ( rule__TypeRacerEvent__RetriesAssignment_7 )
            // InternalRealmForgeDsl.g:1212:3: rule__TypeRacerEvent__RetriesAssignment_7
            {
            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__RetriesAssignment_7();

            state._fsp--;


            }

             after(grammarAccess.getTypeRacerEventAccess().getRetriesAssignment_7()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__7__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__8"
    // InternalRealmForgeDsl.g:1220:1: rule__TypeRacerEvent__Group__8 : rule__TypeRacerEvent__Group__8__Impl rule__TypeRacerEvent__Group__9 ;
    public final void rule__TypeRacerEvent__Group__8() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1224:1: ( rule__TypeRacerEvent__Group__8__Impl rule__TypeRacerEvent__Group__9 )
            // InternalRealmForgeDsl.g:1225:2: rule__TypeRacerEvent__Group__8__Impl rule__TypeRacerEvent__Group__9
            {
            pushFollow(FOLLOW_3);
            rule__TypeRacerEvent__Group__8__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__9();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__8"


    // $ANTLR start "rule__TypeRacerEvent__Group__8__Impl"
    // InternalRealmForgeDsl.g:1232:1: rule__TypeRacerEvent__Group__8__Impl : ( 'sentence' ) ;
    public final void rule__TypeRacerEvent__Group__8__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1236:1: ( ( 'sentence' ) )
            // InternalRealmForgeDsl.g:1237:1: ( 'sentence' )
            {
            // InternalRealmForgeDsl.g:1237:1: ( 'sentence' )
            // InternalRealmForgeDsl.g:1238:2: 'sentence'
            {
             before(grammarAccess.getTypeRacerEventAccess().getSentenceKeyword_8()); 
            match(input,27,FOLLOW_2); 
             after(grammarAccess.getTypeRacerEventAccess().getSentenceKeyword_8()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__8__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__9"
    // InternalRealmForgeDsl.g:1247:1: rule__TypeRacerEvent__Group__9 : rule__TypeRacerEvent__Group__9__Impl rule__TypeRacerEvent__Group__10 ;
    public final void rule__TypeRacerEvent__Group__9() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1251:1: ( rule__TypeRacerEvent__Group__9__Impl rule__TypeRacerEvent__Group__10 )
            // InternalRealmForgeDsl.g:1252:2: rule__TypeRacerEvent__Group__9__Impl rule__TypeRacerEvent__Group__10
            {
            pushFollow(FOLLOW_18);
            rule__TypeRacerEvent__Group__9__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__10();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__9"


    // $ANTLR start "rule__TypeRacerEvent__Group__9__Impl"
    // InternalRealmForgeDsl.g:1259:1: rule__TypeRacerEvent__Group__9__Impl : ( ( rule__TypeRacerEvent__SentenceAssignment_9 ) ) ;
    public final void rule__TypeRacerEvent__Group__9__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1263:1: ( ( ( rule__TypeRacerEvent__SentenceAssignment_9 ) ) )
            // InternalRealmForgeDsl.g:1264:1: ( ( rule__TypeRacerEvent__SentenceAssignment_9 ) )
            {
            // InternalRealmForgeDsl.g:1264:1: ( ( rule__TypeRacerEvent__SentenceAssignment_9 ) )
            // InternalRealmForgeDsl.g:1265:2: ( rule__TypeRacerEvent__SentenceAssignment_9 )
            {
             before(grammarAccess.getTypeRacerEventAccess().getSentenceAssignment_9()); 
            // InternalRealmForgeDsl.g:1266:2: ( rule__TypeRacerEvent__SentenceAssignment_9 )
            // InternalRealmForgeDsl.g:1266:3: rule__TypeRacerEvent__SentenceAssignment_9
            {
            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__SentenceAssignment_9();

            state._fsp--;


            }

             after(grammarAccess.getTypeRacerEventAccess().getSentenceAssignment_9()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__9__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__10"
    // InternalRealmForgeDsl.g:1274:1: rule__TypeRacerEvent__Group__10 : rule__TypeRacerEvent__Group__10__Impl rule__TypeRacerEvent__Group__11 ;
    public final void rule__TypeRacerEvent__Group__10() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1278:1: ( rule__TypeRacerEvent__Group__10__Impl rule__TypeRacerEvent__Group__11 )
            // InternalRealmForgeDsl.g:1279:2: rule__TypeRacerEvent__Group__10__Impl rule__TypeRacerEvent__Group__11
            {
            pushFollow(FOLLOW_12);
            rule__TypeRacerEvent__Group__10__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__11();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__10"


    // $ANTLR start "rule__TypeRacerEvent__Group__10__Impl"
    // InternalRealmForgeDsl.g:1286:1: rule__TypeRacerEvent__Group__10__Impl : ( 'result' ) ;
    public final void rule__TypeRacerEvent__Group__10__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1290:1: ( ( 'result' ) )
            // InternalRealmForgeDsl.g:1291:1: ( 'result' )
            {
            // InternalRealmForgeDsl.g:1291:1: ( 'result' )
            // InternalRealmForgeDsl.g:1292:2: 'result'
            {
             before(grammarAccess.getTypeRacerEventAccess().getResultKeyword_10()); 
            match(input,28,FOLLOW_2); 
             after(grammarAccess.getTypeRacerEventAccess().getResultKeyword_10()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__10__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__11"
    // InternalRealmForgeDsl.g:1301:1: rule__TypeRacerEvent__Group__11 : rule__TypeRacerEvent__Group__11__Impl rule__TypeRacerEvent__Group__12 ;
    public final void rule__TypeRacerEvent__Group__11() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1305:1: ( rule__TypeRacerEvent__Group__11__Impl rule__TypeRacerEvent__Group__12 )
            // InternalRealmForgeDsl.g:1306:2: rule__TypeRacerEvent__Group__11__Impl rule__TypeRacerEvent__Group__12
            {
            pushFollow(FOLLOW_19);
            rule__TypeRacerEvent__Group__11__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__12();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__11"


    // $ANTLR start "rule__TypeRacerEvent__Group__11__Impl"
    // InternalRealmForgeDsl.g:1313:1: rule__TypeRacerEvent__Group__11__Impl : ( ( rule__TypeRacerEvent__ResultAssignment_11 ) ) ;
    public final void rule__TypeRacerEvent__Group__11__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1317:1: ( ( ( rule__TypeRacerEvent__ResultAssignment_11 ) ) )
            // InternalRealmForgeDsl.g:1318:1: ( ( rule__TypeRacerEvent__ResultAssignment_11 ) )
            {
            // InternalRealmForgeDsl.g:1318:1: ( ( rule__TypeRacerEvent__ResultAssignment_11 ) )
            // InternalRealmForgeDsl.g:1319:2: ( rule__TypeRacerEvent__ResultAssignment_11 )
            {
             before(grammarAccess.getTypeRacerEventAccess().getResultAssignment_11()); 
            // InternalRealmForgeDsl.g:1320:2: ( rule__TypeRacerEvent__ResultAssignment_11 )
            // InternalRealmForgeDsl.g:1320:3: rule__TypeRacerEvent__ResultAssignment_11
            {
            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__ResultAssignment_11();

            state._fsp--;


            }

             after(grammarAccess.getTypeRacerEventAccess().getResultAssignment_11()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__11__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group__12"
    // InternalRealmForgeDsl.g:1328:1: rule__TypeRacerEvent__Group__12 : rule__TypeRacerEvent__Group__12__Impl ;
    public final void rule__TypeRacerEvent__Group__12() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1332:1: ( rule__TypeRacerEvent__Group__12__Impl )
            // InternalRealmForgeDsl.g:1333:2: rule__TypeRacerEvent__Group__12__Impl
            {
            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group__12__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__12"


    // $ANTLR start "rule__TypeRacerEvent__Group__12__Impl"
    // InternalRealmForgeDsl.g:1339:1: rule__TypeRacerEvent__Group__12__Impl : ( '}' ) ;
    public final void rule__TypeRacerEvent__Group__12__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1343:1: ( ( '}' ) )
            // InternalRealmForgeDsl.g:1344:1: ( '}' )
            {
            // InternalRealmForgeDsl.g:1344:1: ( '}' )
            // InternalRealmForgeDsl.g:1345:2: '}'
            {
             before(grammarAccess.getTypeRacerEventAccess().getRightCurlyBracketKeyword_12()); 
            match(input,22,FOLLOW_2); 
             after(grammarAccess.getTypeRacerEventAccess().getRightCurlyBracketKeyword_12()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group__12__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group_5__0"
    // InternalRealmForgeDsl.g:1355:1: rule__TypeRacerEvent__Group_5__0 : rule__TypeRacerEvent__Group_5__0__Impl rule__TypeRacerEvent__Group_5__1 ;
    public final void rule__TypeRacerEvent__Group_5__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1359:1: ( rule__TypeRacerEvent__Group_5__0__Impl rule__TypeRacerEvent__Group_5__1 )
            // InternalRealmForgeDsl.g:1360:2: rule__TypeRacerEvent__Group_5__0__Impl rule__TypeRacerEvent__Group_5__1
            {
            pushFollow(FOLLOW_20);
            rule__TypeRacerEvent__Group_5__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group_5__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group_5__0"


    // $ANTLR start "rule__TypeRacerEvent__Group_5__0__Impl"
    // InternalRealmForgeDsl.g:1367:1: rule__TypeRacerEvent__Group_5__0__Impl : ( 'timeLimit' ) ;
    public final void rule__TypeRacerEvent__Group_5__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1371:1: ( ( 'timeLimit' ) )
            // InternalRealmForgeDsl.g:1372:1: ( 'timeLimit' )
            {
            // InternalRealmForgeDsl.g:1372:1: ( 'timeLimit' )
            // InternalRealmForgeDsl.g:1373:2: 'timeLimit'
            {
             before(grammarAccess.getTypeRacerEventAccess().getTimeLimitKeyword_5_0()); 
            match(input,29,FOLLOW_2); 
             after(grammarAccess.getTypeRacerEventAccess().getTimeLimitKeyword_5_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group_5__0__Impl"


    // $ANTLR start "rule__TypeRacerEvent__Group_5__1"
    // InternalRealmForgeDsl.g:1382:1: rule__TypeRacerEvent__Group_5__1 : rule__TypeRacerEvent__Group_5__1__Impl ;
    public final void rule__TypeRacerEvent__Group_5__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1386:1: ( rule__TypeRacerEvent__Group_5__1__Impl )
            // InternalRealmForgeDsl.g:1387:2: rule__TypeRacerEvent__Group_5__1__Impl
            {
            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__Group_5__1__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group_5__1"


    // $ANTLR start "rule__TypeRacerEvent__Group_5__1__Impl"
    // InternalRealmForgeDsl.g:1393:1: rule__TypeRacerEvent__Group_5__1__Impl : ( ( rule__TypeRacerEvent__TimeLimitAssignment_5_1 ) ) ;
    public final void rule__TypeRacerEvent__Group_5__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1397:1: ( ( ( rule__TypeRacerEvent__TimeLimitAssignment_5_1 ) ) )
            // InternalRealmForgeDsl.g:1398:1: ( ( rule__TypeRacerEvent__TimeLimitAssignment_5_1 ) )
            {
            // InternalRealmForgeDsl.g:1398:1: ( ( rule__TypeRacerEvent__TimeLimitAssignment_5_1 ) )
            // InternalRealmForgeDsl.g:1399:2: ( rule__TypeRacerEvent__TimeLimitAssignment_5_1 )
            {
             before(grammarAccess.getTypeRacerEventAccess().getTimeLimitAssignment_5_1()); 
            // InternalRealmForgeDsl.g:1400:2: ( rule__TypeRacerEvent__TimeLimitAssignment_5_1 )
            // InternalRealmForgeDsl.g:1400:3: rule__TypeRacerEvent__TimeLimitAssignment_5_1
            {
            pushFollow(FOLLOW_2);
            rule__TypeRacerEvent__TimeLimitAssignment_5_1();

            state._fsp--;


            }

             after(grammarAccess.getTypeRacerEventAccess().getTimeLimitAssignment_5_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__Group_5__1__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__0"
    // InternalRealmForgeDsl.g:1409:1: rule__QuestionEvent__Group__0 : rule__QuestionEvent__Group__0__Impl rule__QuestionEvent__Group__1 ;
    public final void rule__QuestionEvent__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1413:1: ( rule__QuestionEvent__Group__0__Impl rule__QuestionEvent__Group__1 )
            // InternalRealmForgeDsl.g:1414:2: rule__QuestionEvent__Group__0__Impl rule__QuestionEvent__Group__1
            {
            pushFollow(FOLLOW_12);
            rule__QuestionEvent__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__0"


    // $ANTLR start "rule__QuestionEvent__Group__0__Impl"
    // InternalRealmForgeDsl.g:1421:1: rule__QuestionEvent__Group__0__Impl : ( 'QuestionEvent' ) ;
    public final void rule__QuestionEvent__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1425:1: ( ( 'QuestionEvent' ) )
            // InternalRealmForgeDsl.g:1426:1: ( 'QuestionEvent' )
            {
            // InternalRealmForgeDsl.g:1426:1: ( 'QuestionEvent' )
            // InternalRealmForgeDsl.g:1427:2: 'QuestionEvent'
            {
             before(grammarAccess.getQuestionEventAccess().getQuestionEventKeyword_0()); 
            match(input,30,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getQuestionEventKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__0__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__1"
    // InternalRealmForgeDsl.g:1436:1: rule__QuestionEvent__Group__1 : rule__QuestionEvent__Group__1__Impl rule__QuestionEvent__Group__2 ;
    public final void rule__QuestionEvent__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1440:1: ( rule__QuestionEvent__Group__1__Impl rule__QuestionEvent__Group__2 )
            // InternalRealmForgeDsl.g:1441:2: rule__QuestionEvent__Group__1__Impl rule__QuestionEvent__Group__2
            {
            pushFollow(FOLLOW_21);
            rule__QuestionEvent__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__1"


    // $ANTLR start "rule__QuestionEvent__Group__1__Impl"
    // InternalRealmForgeDsl.g:1448:1: rule__QuestionEvent__Group__1__Impl : ( '{' ) ;
    public final void rule__QuestionEvent__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1452:1: ( ( '{' ) )
            // InternalRealmForgeDsl.g:1453:1: ( '{' )
            {
            // InternalRealmForgeDsl.g:1453:1: ( '{' )
            // InternalRealmForgeDsl.g:1454:2: '{'
            {
             before(grammarAccess.getQuestionEventAccess().getLeftCurlyBracketKeyword_1()); 
            match(input,21,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getLeftCurlyBracketKeyword_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__1__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__2"
    // InternalRealmForgeDsl.g:1463:1: rule__QuestionEvent__Group__2 : rule__QuestionEvent__Group__2__Impl rule__QuestionEvent__Group__3 ;
    public final void rule__QuestionEvent__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1467:1: ( rule__QuestionEvent__Group__2__Impl rule__QuestionEvent__Group__3 )
            // InternalRealmForgeDsl.g:1468:2: rule__QuestionEvent__Group__2__Impl rule__QuestionEvent__Group__3
            {
            pushFollow(FOLLOW_15);
            rule__QuestionEvent__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__2"


    // $ANTLR start "rule__QuestionEvent__Group__2__Impl"
    // InternalRealmForgeDsl.g:1475:1: rule__QuestionEvent__Group__2__Impl : ( 'difficulty' ) ;
    public final void rule__QuestionEvent__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1479:1: ( ( 'difficulty' ) )
            // InternalRealmForgeDsl.g:1480:1: ( 'difficulty' )
            {
            // InternalRealmForgeDsl.g:1480:1: ( 'difficulty' )
            // InternalRealmForgeDsl.g:1481:2: 'difficulty'
            {
             before(grammarAccess.getQuestionEventAccess().getDifficultyKeyword_2()); 
            match(input,25,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getDifficultyKeyword_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__2__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__3"
    // InternalRealmForgeDsl.g:1490:1: rule__QuestionEvent__Group__3 : rule__QuestionEvent__Group__3__Impl rule__QuestionEvent__Group__4 ;
    public final void rule__QuestionEvent__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1494:1: ( rule__QuestionEvent__Group__3__Impl rule__QuestionEvent__Group__4 )
            // InternalRealmForgeDsl.g:1495:2: rule__QuestionEvent__Group__3__Impl rule__QuestionEvent__Group__4
            {
            pushFollow(FOLLOW_16);
            rule__QuestionEvent__Group__3__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__4();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__3"


    // $ANTLR start "rule__QuestionEvent__Group__3__Impl"
    // InternalRealmForgeDsl.g:1502:1: rule__QuestionEvent__Group__3__Impl : ( ( rule__QuestionEvent__DifficultyAssignment_3 ) ) ;
    public final void rule__QuestionEvent__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1506:1: ( ( ( rule__QuestionEvent__DifficultyAssignment_3 ) ) )
            // InternalRealmForgeDsl.g:1507:1: ( ( rule__QuestionEvent__DifficultyAssignment_3 ) )
            {
            // InternalRealmForgeDsl.g:1507:1: ( ( rule__QuestionEvent__DifficultyAssignment_3 ) )
            // InternalRealmForgeDsl.g:1508:2: ( rule__QuestionEvent__DifficultyAssignment_3 )
            {
             before(grammarAccess.getQuestionEventAccess().getDifficultyAssignment_3()); 
            // InternalRealmForgeDsl.g:1509:2: ( rule__QuestionEvent__DifficultyAssignment_3 )
            // InternalRealmForgeDsl.g:1509:3: rule__QuestionEvent__DifficultyAssignment_3
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__DifficultyAssignment_3();

            state._fsp--;


            }

             after(grammarAccess.getQuestionEventAccess().getDifficultyAssignment_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__3__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__4"
    // InternalRealmForgeDsl.g:1517:1: rule__QuestionEvent__Group__4 : rule__QuestionEvent__Group__4__Impl rule__QuestionEvent__Group__5 ;
    public final void rule__QuestionEvent__Group__4() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1521:1: ( rule__QuestionEvent__Group__4__Impl rule__QuestionEvent__Group__5 )
            // InternalRealmForgeDsl.g:1522:2: rule__QuestionEvent__Group__4__Impl rule__QuestionEvent__Group__5
            {
            pushFollow(FOLLOW_16);
            rule__QuestionEvent__Group__4__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__5();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__4"


    // $ANTLR start "rule__QuestionEvent__Group__4__Impl"
    // InternalRealmForgeDsl.g:1529:1: rule__QuestionEvent__Group__4__Impl : ( ( rule__QuestionEvent__Group_4__0 )? ) ;
    public final void rule__QuestionEvent__Group__4__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1533:1: ( ( ( rule__QuestionEvent__Group_4__0 )? ) )
            // InternalRealmForgeDsl.g:1534:1: ( ( rule__QuestionEvent__Group_4__0 )? )
            {
            // InternalRealmForgeDsl.g:1534:1: ( ( rule__QuestionEvent__Group_4__0 )? )
            // InternalRealmForgeDsl.g:1535:2: ( rule__QuestionEvent__Group_4__0 )?
            {
             before(grammarAccess.getQuestionEventAccess().getGroup_4()); 
            // InternalRealmForgeDsl.g:1536:2: ( rule__QuestionEvent__Group_4__0 )?
            int alt14=2;
            int LA14_0 = input.LA(1);

            if ( (LA14_0==29) ) {
                alt14=1;
            }
            switch (alt14) {
                case 1 :
                    // InternalRealmForgeDsl.g:1536:3: rule__QuestionEvent__Group_4__0
                    {
                    pushFollow(FOLLOW_2);
                    rule__QuestionEvent__Group_4__0();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getQuestionEventAccess().getGroup_4()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__4__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__5"
    // InternalRealmForgeDsl.g:1544:1: rule__QuestionEvent__Group__5 : rule__QuestionEvent__Group__5__Impl rule__QuestionEvent__Group__6 ;
    public final void rule__QuestionEvent__Group__5() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1548:1: ( rule__QuestionEvent__Group__5__Impl rule__QuestionEvent__Group__6 )
            // InternalRealmForgeDsl.g:1549:2: rule__QuestionEvent__Group__5__Impl rule__QuestionEvent__Group__6
            {
            pushFollow(FOLLOW_11);
            rule__QuestionEvent__Group__5__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__6();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__5"


    // $ANTLR start "rule__QuestionEvent__Group__5__Impl"
    // InternalRealmForgeDsl.g:1556:1: rule__QuestionEvent__Group__5__Impl : ( 'retries' ) ;
    public final void rule__QuestionEvent__Group__5__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1560:1: ( ( 'retries' ) )
            // InternalRealmForgeDsl.g:1561:1: ( 'retries' )
            {
            // InternalRealmForgeDsl.g:1561:1: ( 'retries' )
            // InternalRealmForgeDsl.g:1562:2: 'retries'
            {
             before(grammarAccess.getQuestionEventAccess().getRetriesKeyword_5()); 
            match(input,26,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getRetriesKeyword_5()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__5__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__6"
    // InternalRealmForgeDsl.g:1571:1: rule__QuestionEvent__Group__6 : rule__QuestionEvent__Group__6__Impl rule__QuestionEvent__Group__7 ;
    public final void rule__QuestionEvent__Group__6() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1575:1: ( rule__QuestionEvent__Group__6__Impl rule__QuestionEvent__Group__7 )
            // InternalRealmForgeDsl.g:1576:2: rule__QuestionEvent__Group__6__Impl rule__QuestionEvent__Group__7
            {
            pushFollow(FOLLOW_22);
            rule__QuestionEvent__Group__6__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__7();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__6"


    // $ANTLR start "rule__QuestionEvent__Group__6__Impl"
    // InternalRealmForgeDsl.g:1583:1: rule__QuestionEvent__Group__6__Impl : ( ( rule__QuestionEvent__RetriesAssignment_6 ) ) ;
    public final void rule__QuestionEvent__Group__6__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1587:1: ( ( ( rule__QuestionEvent__RetriesAssignment_6 ) ) )
            // InternalRealmForgeDsl.g:1588:1: ( ( rule__QuestionEvent__RetriesAssignment_6 ) )
            {
            // InternalRealmForgeDsl.g:1588:1: ( ( rule__QuestionEvent__RetriesAssignment_6 ) )
            // InternalRealmForgeDsl.g:1589:2: ( rule__QuestionEvent__RetriesAssignment_6 )
            {
             before(grammarAccess.getQuestionEventAccess().getRetriesAssignment_6()); 
            // InternalRealmForgeDsl.g:1590:2: ( rule__QuestionEvent__RetriesAssignment_6 )
            // InternalRealmForgeDsl.g:1590:3: rule__QuestionEvent__RetriesAssignment_6
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__RetriesAssignment_6();

            state._fsp--;


            }

             after(grammarAccess.getQuestionEventAccess().getRetriesAssignment_6()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__6__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__7"
    // InternalRealmForgeDsl.g:1598:1: rule__QuestionEvent__Group__7 : rule__QuestionEvent__Group__7__Impl rule__QuestionEvent__Group__8 ;
    public final void rule__QuestionEvent__Group__7() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1602:1: ( rule__QuestionEvent__Group__7__Impl rule__QuestionEvent__Group__8 )
            // InternalRealmForgeDsl.g:1603:2: rule__QuestionEvent__Group__7__Impl rule__QuestionEvent__Group__8
            {
            pushFollow(FOLLOW_3);
            rule__QuestionEvent__Group__7__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__8();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__7"


    // $ANTLR start "rule__QuestionEvent__Group__7__Impl"
    // InternalRealmForgeDsl.g:1610:1: rule__QuestionEvent__Group__7__Impl : ( 'question' ) ;
    public final void rule__QuestionEvent__Group__7__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1614:1: ( ( 'question' ) )
            // InternalRealmForgeDsl.g:1615:1: ( 'question' )
            {
            // InternalRealmForgeDsl.g:1615:1: ( 'question' )
            // InternalRealmForgeDsl.g:1616:2: 'question'
            {
             before(grammarAccess.getQuestionEventAccess().getQuestionKeyword_7()); 
            match(input,31,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getQuestionKeyword_7()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__7__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__8"
    // InternalRealmForgeDsl.g:1625:1: rule__QuestionEvent__Group__8 : rule__QuestionEvent__Group__8__Impl rule__QuestionEvent__Group__9 ;
    public final void rule__QuestionEvent__Group__8() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1629:1: ( rule__QuestionEvent__Group__8__Impl rule__QuestionEvent__Group__9 )
            // InternalRealmForgeDsl.g:1630:2: rule__QuestionEvent__Group__8__Impl rule__QuestionEvent__Group__9
            {
            pushFollow(FOLLOW_18);
            rule__QuestionEvent__Group__8__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__9();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__8"


    // $ANTLR start "rule__QuestionEvent__Group__8__Impl"
    // InternalRealmForgeDsl.g:1637:1: rule__QuestionEvent__Group__8__Impl : ( ( rule__QuestionEvent__QuestionAssignment_8 ) ) ;
    public final void rule__QuestionEvent__Group__8__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1641:1: ( ( ( rule__QuestionEvent__QuestionAssignment_8 ) ) )
            // InternalRealmForgeDsl.g:1642:1: ( ( rule__QuestionEvent__QuestionAssignment_8 ) )
            {
            // InternalRealmForgeDsl.g:1642:1: ( ( rule__QuestionEvent__QuestionAssignment_8 ) )
            // InternalRealmForgeDsl.g:1643:2: ( rule__QuestionEvent__QuestionAssignment_8 )
            {
             before(grammarAccess.getQuestionEventAccess().getQuestionAssignment_8()); 
            // InternalRealmForgeDsl.g:1644:2: ( rule__QuestionEvent__QuestionAssignment_8 )
            // InternalRealmForgeDsl.g:1644:3: rule__QuestionEvent__QuestionAssignment_8
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__QuestionAssignment_8();

            state._fsp--;


            }

             after(grammarAccess.getQuestionEventAccess().getQuestionAssignment_8()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__8__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__9"
    // InternalRealmForgeDsl.g:1652:1: rule__QuestionEvent__Group__9 : rule__QuestionEvent__Group__9__Impl rule__QuestionEvent__Group__10 ;
    public final void rule__QuestionEvent__Group__9() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1656:1: ( rule__QuestionEvent__Group__9__Impl rule__QuestionEvent__Group__10 )
            // InternalRealmForgeDsl.g:1657:2: rule__QuestionEvent__Group__9__Impl rule__QuestionEvent__Group__10
            {
            pushFollow(FOLLOW_12);
            rule__QuestionEvent__Group__9__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__10();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__9"


    // $ANTLR start "rule__QuestionEvent__Group__9__Impl"
    // InternalRealmForgeDsl.g:1664:1: rule__QuestionEvent__Group__9__Impl : ( 'result' ) ;
    public final void rule__QuestionEvent__Group__9__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1668:1: ( ( 'result' ) )
            // InternalRealmForgeDsl.g:1669:1: ( 'result' )
            {
            // InternalRealmForgeDsl.g:1669:1: ( 'result' )
            // InternalRealmForgeDsl.g:1670:2: 'result'
            {
             before(grammarAccess.getQuestionEventAccess().getResultKeyword_9()); 
            match(input,28,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getResultKeyword_9()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__9__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__10"
    // InternalRealmForgeDsl.g:1679:1: rule__QuestionEvent__Group__10 : rule__QuestionEvent__Group__10__Impl rule__QuestionEvent__Group__11 ;
    public final void rule__QuestionEvent__Group__10() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1683:1: ( rule__QuestionEvent__Group__10__Impl rule__QuestionEvent__Group__11 )
            // InternalRealmForgeDsl.g:1684:2: rule__QuestionEvent__Group__10__Impl rule__QuestionEvent__Group__11
            {
            pushFollow(FOLLOW_23);
            rule__QuestionEvent__Group__10__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__11();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__10"


    // $ANTLR start "rule__QuestionEvent__Group__10__Impl"
    // InternalRealmForgeDsl.g:1691:1: rule__QuestionEvent__Group__10__Impl : ( ( rule__QuestionEvent__ResultAssignment_10 ) ) ;
    public final void rule__QuestionEvent__Group__10__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1695:1: ( ( ( rule__QuestionEvent__ResultAssignment_10 ) ) )
            // InternalRealmForgeDsl.g:1696:1: ( ( rule__QuestionEvent__ResultAssignment_10 ) )
            {
            // InternalRealmForgeDsl.g:1696:1: ( ( rule__QuestionEvent__ResultAssignment_10 ) )
            // InternalRealmForgeDsl.g:1697:2: ( rule__QuestionEvent__ResultAssignment_10 )
            {
             before(grammarAccess.getQuestionEventAccess().getResultAssignment_10()); 
            // InternalRealmForgeDsl.g:1698:2: ( rule__QuestionEvent__ResultAssignment_10 )
            // InternalRealmForgeDsl.g:1698:3: rule__QuestionEvent__ResultAssignment_10
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__ResultAssignment_10();

            state._fsp--;


            }

             after(grammarAccess.getQuestionEventAccess().getResultAssignment_10()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__10__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__11"
    // InternalRealmForgeDsl.g:1706:1: rule__QuestionEvent__Group__11 : rule__QuestionEvent__Group__11__Impl rule__QuestionEvent__Group__12 ;
    public final void rule__QuestionEvent__Group__11() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1710:1: ( rule__QuestionEvent__Group__11__Impl rule__QuestionEvent__Group__12 )
            // InternalRealmForgeDsl.g:1711:2: rule__QuestionEvent__Group__11__Impl rule__QuestionEvent__Group__12
            {
            pushFollow(FOLLOW_12);
            rule__QuestionEvent__Group__11__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__12();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__11"


    // $ANTLR start "rule__QuestionEvent__Group__11__Impl"
    // InternalRealmForgeDsl.g:1718:1: rule__QuestionEvent__Group__11__Impl : ( 'options' ) ;
    public final void rule__QuestionEvent__Group__11__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1722:1: ( ( 'options' ) )
            // InternalRealmForgeDsl.g:1723:1: ( 'options' )
            {
            // InternalRealmForgeDsl.g:1723:1: ( 'options' )
            // InternalRealmForgeDsl.g:1724:2: 'options'
            {
             before(grammarAccess.getQuestionEventAccess().getOptionsKeyword_11()); 
            match(input,32,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getOptionsKeyword_11()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__11__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__12"
    // InternalRealmForgeDsl.g:1733:1: rule__QuestionEvent__Group__12 : rule__QuestionEvent__Group__12__Impl rule__QuestionEvent__Group__13 ;
    public final void rule__QuestionEvent__Group__12() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1737:1: ( rule__QuestionEvent__Group__12__Impl rule__QuestionEvent__Group__13 )
            // InternalRealmForgeDsl.g:1738:2: rule__QuestionEvent__Group__12__Impl rule__QuestionEvent__Group__13
            {
            pushFollow(FOLLOW_12);
            rule__QuestionEvent__Group__12__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__13();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__12"


    // $ANTLR start "rule__QuestionEvent__Group__12__Impl"
    // InternalRealmForgeDsl.g:1745:1: rule__QuestionEvent__Group__12__Impl : ( '{' ) ;
    public final void rule__QuestionEvent__Group__12__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1749:1: ( ( '{' ) )
            // InternalRealmForgeDsl.g:1750:1: ( '{' )
            {
            // InternalRealmForgeDsl.g:1750:1: ( '{' )
            // InternalRealmForgeDsl.g:1751:2: '{'
            {
             before(grammarAccess.getQuestionEventAccess().getLeftCurlyBracketKeyword_12()); 
            match(input,21,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getLeftCurlyBracketKeyword_12()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__12__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__13"
    // InternalRealmForgeDsl.g:1760:1: rule__QuestionEvent__Group__13 : rule__QuestionEvent__Group__13__Impl rule__QuestionEvent__Group__14 ;
    public final void rule__QuestionEvent__Group__13() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1764:1: ( rule__QuestionEvent__Group__13__Impl rule__QuestionEvent__Group__14 )
            // InternalRealmForgeDsl.g:1765:2: rule__QuestionEvent__Group__13__Impl rule__QuestionEvent__Group__14
            {
            pushFollow(FOLLOW_24);
            rule__QuestionEvent__Group__13__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__14();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__13"


    // $ANTLR start "rule__QuestionEvent__Group__13__Impl"
    // InternalRealmForgeDsl.g:1772:1: rule__QuestionEvent__Group__13__Impl : ( ( rule__QuestionEvent__OptionsAssignment_13 ) ) ;
    public final void rule__QuestionEvent__Group__13__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1776:1: ( ( ( rule__QuestionEvent__OptionsAssignment_13 ) ) )
            // InternalRealmForgeDsl.g:1777:1: ( ( rule__QuestionEvent__OptionsAssignment_13 ) )
            {
            // InternalRealmForgeDsl.g:1777:1: ( ( rule__QuestionEvent__OptionsAssignment_13 ) )
            // InternalRealmForgeDsl.g:1778:2: ( rule__QuestionEvent__OptionsAssignment_13 )
            {
             before(grammarAccess.getQuestionEventAccess().getOptionsAssignment_13()); 
            // InternalRealmForgeDsl.g:1779:2: ( rule__QuestionEvent__OptionsAssignment_13 )
            // InternalRealmForgeDsl.g:1779:3: rule__QuestionEvent__OptionsAssignment_13
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__OptionsAssignment_13();

            state._fsp--;


            }

             after(grammarAccess.getQuestionEventAccess().getOptionsAssignment_13()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__13__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__14"
    // InternalRealmForgeDsl.g:1787:1: rule__QuestionEvent__Group__14 : rule__QuestionEvent__Group__14__Impl rule__QuestionEvent__Group__15 ;
    public final void rule__QuestionEvent__Group__14() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1791:1: ( rule__QuestionEvent__Group__14__Impl rule__QuestionEvent__Group__15 )
            // InternalRealmForgeDsl.g:1792:2: rule__QuestionEvent__Group__14__Impl rule__QuestionEvent__Group__15
            {
            pushFollow(FOLLOW_24);
            rule__QuestionEvent__Group__14__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__15();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__14"


    // $ANTLR start "rule__QuestionEvent__Group__14__Impl"
    // InternalRealmForgeDsl.g:1799:1: rule__QuestionEvent__Group__14__Impl : ( ( rule__QuestionEvent__Group_14__0 )* ) ;
    public final void rule__QuestionEvent__Group__14__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1803:1: ( ( ( rule__QuestionEvent__Group_14__0 )* ) )
            // InternalRealmForgeDsl.g:1804:1: ( ( rule__QuestionEvent__Group_14__0 )* )
            {
            // InternalRealmForgeDsl.g:1804:1: ( ( rule__QuestionEvent__Group_14__0 )* )
            // InternalRealmForgeDsl.g:1805:2: ( rule__QuestionEvent__Group_14__0 )*
            {
             before(grammarAccess.getQuestionEventAccess().getGroup_14()); 
            // InternalRealmForgeDsl.g:1806:2: ( rule__QuestionEvent__Group_14__0 )*
            loop15:
            do {
                int alt15=2;
                int LA15_0 = input.LA(1);

                if ( (LA15_0==33) ) {
                    alt15=1;
                }


                switch (alt15) {
            	case 1 :
            	    // InternalRealmForgeDsl.g:1806:3: rule__QuestionEvent__Group_14__0
            	    {
            	    pushFollow(FOLLOW_25);
            	    rule__QuestionEvent__Group_14__0();

            	    state._fsp--;


            	    }
            	    break;

            	default :
            	    break loop15;
                }
            } while (true);

             after(grammarAccess.getQuestionEventAccess().getGroup_14()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__14__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__15"
    // InternalRealmForgeDsl.g:1814:1: rule__QuestionEvent__Group__15 : rule__QuestionEvent__Group__15__Impl rule__QuestionEvent__Group__16 ;
    public final void rule__QuestionEvent__Group__15() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1818:1: ( rule__QuestionEvent__Group__15__Impl rule__QuestionEvent__Group__16 )
            // InternalRealmForgeDsl.g:1819:2: rule__QuestionEvent__Group__15__Impl rule__QuestionEvent__Group__16
            {
            pushFollow(FOLLOW_19);
            rule__QuestionEvent__Group__15__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__16();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__15"


    // $ANTLR start "rule__QuestionEvent__Group__15__Impl"
    // InternalRealmForgeDsl.g:1826:1: rule__QuestionEvent__Group__15__Impl : ( '}' ) ;
    public final void rule__QuestionEvent__Group__15__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1830:1: ( ( '}' ) )
            // InternalRealmForgeDsl.g:1831:1: ( '}' )
            {
            // InternalRealmForgeDsl.g:1831:1: ( '}' )
            // InternalRealmForgeDsl.g:1832:2: '}'
            {
             before(grammarAccess.getQuestionEventAccess().getRightCurlyBracketKeyword_15()); 
            match(input,22,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getRightCurlyBracketKeyword_15()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__15__Impl"


    // $ANTLR start "rule__QuestionEvent__Group__16"
    // InternalRealmForgeDsl.g:1841:1: rule__QuestionEvent__Group__16 : rule__QuestionEvent__Group__16__Impl ;
    public final void rule__QuestionEvent__Group__16() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1845:1: ( rule__QuestionEvent__Group__16__Impl )
            // InternalRealmForgeDsl.g:1846:2: rule__QuestionEvent__Group__16__Impl
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group__16__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__16"


    // $ANTLR start "rule__QuestionEvent__Group__16__Impl"
    // InternalRealmForgeDsl.g:1852:1: rule__QuestionEvent__Group__16__Impl : ( '}' ) ;
    public final void rule__QuestionEvent__Group__16__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1856:1: ( ( '}' ) )
            // InternalRealmForgeDsl.g:1857:1: ( '}' )
            {
            // InternalRealmForgeDsl.g:1857:1: ( '}' )
            // InternalRealmForgeDsl.g:1858:2: '}'
            {
             before(grammarAccess.getQuestionEventAccess().getRightCurlyBracketKeyword_16()); 
            match(input,22,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getRightCurlyBracketKeyword_16()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group__16__Impl"


    // $ANTLR start "rule__QuestionEvent__Group_4__0"
    // InternalRealmForgeDsl.g:1868:1: rule__QuestionEvent__Group_4__0 : rule__QuestionEvent__Group_4__0__Impl rule__QuestionEvent__Group_4__1 ;
    public final void rule__QuestionEvent__Group_4__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1872:1: ( rule__QuestionEvent__Group_4__0__Impl rule__QuestionEvent__Group_4__1 )
            // InternalRealmForgeDsl.g:1873:2: rule__QuestionEvent__Group_4__0__Impl rule__QuestionEvent__Group_4__1
            {
            pushFollow(FOLLOW_20);
            rule__QuestionEvent__Group_4__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group_4__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group_4__0"


    // $ANTLR start "rule__QuestionEvent__Group_4__0__Impl"
    // InternalRealmForgeDsl.g:1880:1: rule__QuestionEvent__Group_4__0__Impl : ( 'timeLimit' ) ;
    public final void rule__QuestionEvent__Group_4__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1884:1: ( ( 'timeLimit' ) )
            // InternalRealmForgeDsl.g:1885:1: ( 'timeLimit' )
            {
            // InternalRealmForgeDsl.g:1885:1: ( 'timeLimit' )
            // InternalRealmForgeDsl.g:1886:2: 'timeLimit'
            {
             before(grammarAccess.getQuestionEventAccess().getTimeLimitKeyword_4_0()); 
            match(input,29,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getTimeLimitKeyword_4_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group_4__0__Impl"


    // $ANTLR start "rule__QuestionEvent__Group_4__1"
    // InternalRealmForgeDsl.g:1895:1: rule__QuestionEvent__Group_4__1 : rule__QuestionEvent__Group_4__1__Impl ;
    public final void rule__QuestionEvent__Group_4__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1899:1: ( rule__QuestionEvent__Group_4__1__Impl )
            // InternalRealmForgeDsl.g:1900:2: rule__QuestionEvent__Group_4__1__Impl
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group_4__1__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group_4__1"


    // $ANTLR start "rule__QuestionEvent__Group_4__1__Impl"
    // InternalRealmForgeDsl.g:1906:1: rule__QuestionEvent__Group_4__1__Impl : ( ( rule__QuestionEvent__TimeLimitAssignment_4_1 ) ) ;
    public final void rule__QuestionEvent__Group_4__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1910:1: ( ( ( rule__QuestionEvent__TimeLimitAssignment_4_1 ) ) )
            // InternalRealmForgeDsl.g:1911:1: ( ( rule__QuestionEvent__TimeLimitAssignment_4_1 ) )
            {
            // InternalRealmForgeDsl.g:1911:1: ( ( rule__QuestionEvent__TimeLimitAssignment_4_1 ) )
            // InternalRealmForgeDsl.g:1912:2: ( rule__QuestionEvent__TimeLimitAssignment_4_1 )
            {
             before(grammarAccess.getQuestionEventAccess().getTimeLimitAssignment_4_1()); 
            // InternalRealmForgeDsl.g:1913:2: ( rule__QuestionEvent__TimeLimitAssignment_4_1 )
            // InternalRealmForgeDsl.g:1913:3: rule__QuestionEvent__TimeLimitAssignment_4_1
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__TimeLimitAssignment_4_1();

            state._fsp--;


            }

             after(grammarAccess.getQuestionEventAccess().getTimeLimitAssignment_4_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group_4__1__Impl"


    // $ANTLR start "rule__QuestionEvent__Group_14__0"
    // InternalRealmForgeDsl.g:1922:1: rule__QuestionEvent__Group_14__0 : rule__QuestionEvent__Group_14__0__Impl rule__QuestionEvent__Group_14__1 ;
    public final void rule__QuestionEvent__Group_14__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1926:1: ( rule__QuestionEvent__Group_14__0__Impl rule__QuestionEvent__Group_14__1 )
            // InternalRealmForgeDsl.g:1927:2: rule__QuestionEvent__Group_14__0__Impl rule__QuestionEvent__Group_14__1
            {
            pushFollow(FOLLOW_12);
            rule__QuestionEvent__Group_14__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group_14__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group_14__0"


    // $ANTLR start "rule__QuestionEvent__Group_14__0__Impl"
    // InternalRealmForgeDsl.g:1934:1: rule__QuestionEvent__Group_14__0__Impl : ( ',' ) ;
    public final void rule__QuestionEvent__Group_14__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1938:1: ( ( ',' ) )
            // InternalRealmForgeDsl.g:1939:1: ( ',' )
            {
            // InternalRealmForgeDsl.g:1939:1: ( ',' )
            // InternalRealmForgeDsl.g:1940:2: ','
            {
             before(grammarAccess.getQuestionEventAccess().getCommaKeyword_14_0()); 
            match(input,33,FOLLOW_2); 
             after(grammarAccess.getQuestionEventAccess().getCommaKeyword_14_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group_14__0__Impl"


    // $ANTLR start "rule__QuestionEvent__Group_14__1"
    // InternalRealmForgeDsl.g:1949:1: rule__QuestionEvent__Group_14__1 : rule__QuestionEvent__Group_14__1__Impl ;
    public final void rule__QuestionEvent__Group_14__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1953:1: ( rule__QuestionEvent__Group_14__1__Impl )
            // InternalRealmForgeDsl.g:1954:2: rule__QuestionEvent__Group_14__1__Impl
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__Group_14__1__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group_14__1"


    // $ANTLR start "rule__QuestionEvent__Group_14__1__Impl"
    // InternalRealmForgeDsl.g:1960:1: rule__QuestionEvent__Group_14__1__Impl : ( ( rule__QuestionEvent__OptionsAssignment_14_1 ) ) ;
    public final void rule__QuestionEvent__Group_14__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1964:1: ( ( ( rule__QuestionEvent__OptionsAssignment_14_1 ) ) )
            // InternalRealmForgeDsl.g:1965:1: ( ( rule__QuestionEvent__OptionsAssignment_14_1 ) )
            {
            // InternalRealmForgeDsl.g:1965:1: ( ( rule__QuestionEvent__OptionsAssignment_14_1 ) )
            // InternalRealmForgeDsl.g:1966:2: ( rule__QuestionEvent__OptionsAssignment_14_1 )
            {
             before(grammarAccess.getQuestionEventAccess().getOptionsAssignment_14_1()); 
            // InternalRealmForgeDsl.g:1967:2: ( rule__QuestionEvent__OptionsAssignment_14_1 )
            // InternalRealmForgeDsl.g:1967:3: rule__QuestionEvent__OptionsAssignment_14_1
            {
            pushFollow(FOLLOW_2);
            rule__QuestionEvent__OptionsAssignment_14_1();

            state._fsp--;


            }

             after(grammarAccess.getQuestionEventAccess().getOptionsAssignment_14_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__Group_14__1__Impl"


    // $ANTLR start "rule__Option__Group__0"
    // InternalRealmForgeDsl.g:1976:1: rule__Option__Group__0 : rule__Option__Group__0__Impl rule__Option__Group__1 ;
    public final void rule__Option__Group__0() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1980:1: ( rule__Option__Group__0__Impl rule__Option__Group__1 )
            // InternalRealmForgeDsl.g:1981:2: rule__Option__Group__0__Impl rule__Option__Group__1
            {
            pushFollow(FOLLOW_26);
            rule__Option__Group__0__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__Option__Group__1();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__0"


    // $ANTLR start "rule__Option__Group__0__Impl"
    // InternalRealmForgeDsl.g:1988:1: rule__Option__Group__0__Impl : ( '{' ) ;
    public final void rule__Option__Group__0__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:1992:1: ( ( '{' ) )
            // InternalRealmForgeDsl.g:1993:1: ( '{' )
            {
            // InternalRealmForgeDsl.g:1993:1: ( '{' )
            // InternalRealmForgeDsl.g:1994:2: '{'
            {
             before(grammarAccess.getOptionAccess().getLeftCurlyBracketKeyword_0()); 
            match(input,21,FOLLOW_2); 
             after(grammarAccess.getOptionAccess().getLeftCurlyBracketKeyword_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__0__Impl"


    // $ANTLR start "rule__Option__Group__1"
    // InternalRealmForgeDsl.g:2003:1: rule__Option__Group__1 : rule__Option__Group__1__Impl rule__Option__Group__2 ;
    public final void rule__Option__Group__1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2007:1: ( rule__Option__Group__1__Impl rule__Option__Group__2 )
            // InternalRealmForgeDsl.g:2008:2: rule__Option__Group__1__Impl rule__Option__Group__2
            {
            pushFollow(FOLLOW_3);
            rule__Option__Group__1__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__Option__Group__2();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__1"


    // $ANTLR start "rule__Option__Group__1__Impl"
    // InternalRealmForgeDsl.g:2015:1: rule__Option__Group__1__Impl : ( 'text' ) ;
    public final void rule__Option__Group__1__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2019:1: ( ( 'text' ) )
            // InternalRealmForgeDsl.g:2020:1: ( 'text' )
            {
            // InternalRealmForgeDsl.g:2020:1: ( 'text' )
            // InternalRealmForgeDsl.g:2021:2: 'text'
            {
             before(grammarAccess.getOptionAccess().getTextKeyword_1()); 
            match(input,34,FOLLOW_2); 
             after(grammarAccess.getOptionAccess().getTextKeyword_1()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__1__Impl"


    // $ANTLR start "rule__Option__Group__2"
    // InternalRealmForgeDsl.g:2030:1: rule__Option__Group__2 : rule__Option__Group__2__Impl rule__Option__Group__3 ;
    public final void rule__Option__Group__2() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2034:1: ( rule__Option__Group__2__Impl rule__Option__Group__3 )
            // InternalRealmForgeDsl.g:2035:2: rule__Option__Group__2__Impl rule__Option__Group__3
            {
            pushFollow(FOLLOW_27);
            rule__Option__Group__2__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__Option__Group__3();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__2"


    // $ANTLR start "rule__Option__Group__2__Impl"
    // InternalRealmForgeDsl.g:2042:1: rule__Option__Group__2__Impl : ( ( rule__Option__TextAssignment_2 ) ) ;
    public final void rule__Option__Group__2__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2046:1: ( ( ( rule__Option__TextAssignment_2 ) ) )
            // InternalRealmForgeDsl.g:2047:1: ( ( rule__Option__TextAssignment_2 ) )
            {
            // InternalRealmForgeDsl.g:2047:1: ( ( rule__Option__TextAssignment_2 ) )
            // InternalRealmForgeDsl.g:2048:2: ( rule__Option__TextAssignment_2 )
            {
             before(grammarAccess.getOptionAccess().getTextAssignment_2()); 
            // InternalRealmForgeDsl.g:2049:2: ( rule__Option__TextAssignment_2 )
            // InternalRealmForgeDsl.g:2049:3: rule__Option__TextAssignment_2
            {
            pushFollow(FOLLOW_2);
            rule__Option__TextAssignment_2();

            state._fsp--;


            }

             after(grammarAccess.getOptionAccess().getTextAssignment_2()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__2__Impl"


    // $ANTLR start "rule__Option__Group__3"
    // InternalRealmForgeDsl.g:2057:1: rule__Option__Group__3 : rule__Option__Group__3__Impl rule__Option__Group__4 ;
    public final void rule__Option__Group__3() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2061:1: ( rule__Option__Group__3__Impl rule__Option__Group__4 )
            // InternalRealmForgeDsl.g:2062:2: rule__Option__Group__3__Impl rule__Option__Group__4
            {
            pushFollow(FOLLOW_27);
            rule__Option__Group__3__Impl();

            state._fsp--;

            pushFollow(FOLLOW_2);
            rule__Option__Group__4();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__3"


    // $ANTLR start "rule__Option__Group__3__Impl"
    // InternalRealmForgeDsl.g:2069:1: rule__Option__Group__3__Impl : ( ( rule__Option__IsCorrectAnswerAssignment_3 )? ) ;
    public final void rule__Option__Group__3__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2073:1: ( ( ( rule__Option__IsCorrectAnswerAssignment_3 )? ) )
            // InternalRealmForgeDsl.g:2074:1: ( ( rule__Option__IsCorrectAnswerAssignment_3 )? )
            {
            // InternalRealmForgeDsl.g:2074:1: ( ( rule__Option__IsCorrectAnswerAssignment_3 )? )
            // InternalRealmForgeDsl.g:2075:2: ( rule__Option__IsCorrectAnswerAssignment_3 )?
            {
             before(grammarAccess.getOptionAccess().getIsCorrectAnswerAssignment_3()); 
            // InternalRealmForgeDsl.g:2076:2: ( rule__Option__IsCorrectAnswerAssignment_3 )?
            int alt16=2;
            int LA16_0 = input.LA(1);

            if ( (LA16_0==36) ) {
                alt16=1;
            }
            switch (alt16) {
                case 1 :
                    // InternalRealmForgeDsl.g:2076:3: rule__Option__IsCorrectAnswerAssignment_3
                    {
                    pushFollow(FOLLOW_2);
                    rule__Option__IsCorrectAnswerAssignment_3();

                    state._fsp--;


                    }
                    break;

            }

             after(grammarAccess.getOptionAccess().getIsCorrectAnswerAssignment_3()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__3__Impl"


    // $ANTLR start "rule__Option__Group__4"
    // InternalRealmForgeDsl.g:2084:1: rule__Option__Group__4 : rule__Option__Group__4__Impl ;
    public final void rule__Option__Group__4() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2088:1: ( rule__Option__Group__4__Impl )
            // InternalRealmForgeDsl.g:2089:2: rule__Option__Group__4__Impl
            {
            pushFollow(FOLLOW_2);
            rule__Option__Group__4__Impl();

            state._fsp--;


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__4"


    // $ANTLR start "rule__Option__Group__4__Impl"
    // InternalRealmForgeDsl.g:2095:1: rule__Option__Group__4__Impl : ( '}' ) ;
    public final void rule__Option__Group__4__Impl() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2099:1: ( ( '}' ) )
            // InternalRealmForgeDsl.g:2100:1: ( '}' )
            {
            // InternalRealmForgeDsl.g:2100:1: ( '}' )
            // InternalRealmForgeDsl.g:2101:2: '}'
            {
             before(grammarAccess.getOptionAccess().getRightCurlyBracketKeyword_4()); 
            match(input,22,FOLLOW_2); 
             after(grammarAccess.getOptionAccess().getRightCurlyBracketKeyword_4()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__Group__4__Impl"


    // $ANTLR start "rule__EventPack__NameAssignment_1"
    // InternalRealmForgeDsl.g:2111:1: rule__EventPack__NameAssignment_1 : ( ruleEString ) ;
    public final void rule__EventPack__NameAssignment_1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2115:1: ( ( ruleEString ) )
            // InternalRealmForgeDsl.g:2116:2: ( ruleEString )
            {
            // InternalRealmForgeDsl.g:2116:2: ( ruleEString )
            // InternalRealmForgeDsl.g:2117:3: ruleEString
            {
             before(grammarAccess.getEventPackAccess().getNameEStringParserRuleCall_1_0()); 
            pushFollow(FOLLOW_2);
            ruleEString();

            state._fsp--;

             after(grammarAccess.getEventPackAccess().getNameEStringParserRuleCall_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__NameAssignment_1"


    // $ANTLR start "rule__EventPack__DescriptionAssignment_3"
    // InternalRealmForgeDsl.g:2126:1: rule__EventPack__DescriptionAssignment_3 : ( ruleEString ) ;
    public final void rule__EventPack__DescriptionAssignment_3() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2130:1: ( ( ruleEString ) )
            // InternalRealmForgeDsl.g:2131:2: ( ruleEString )
            {
            // InternalRealmForgeDsl.g:2131:2: ( ruleEString )
            // InternalRealmForgeDsl.g:2132:3: ruleEString
            {
             before(grammarAccess.getEventPackAccess().getDescriptionEStringParserRuleCall_3_0()); 
            pushFollow(FOLLOW_2);
            ruleEString();

            state._fsp--;

             after(grammarAccess.getEventPackAccess().getDescriptionEStringParserRuleCall_3_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__DescriptionAssignment_3"


    // $ANTLR start "rule__EventPack__UnitAssignment_5"
    // InternalRealmForgeDsl.g:2141:1: rule__EventPack__UnitAssignment_5 : ( ruleEString ) ;
    public final void rule__EventPack__UnitAssignment_5() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2145:1: ( ( ruleEString ) )
            // InternalRealmForgeDsl.g:2146:2: ( ruleEString )
            {
            // InternalRealmForgeDsl.g:2146:2: ( ruleEString )
            // InternalRealmForgeDsl.g:2147:3: ruleEString
            {
             before(grammarAccess.getEventPackAccess().getUnitEStringParserRuleCall_5_0()); 
            pushFollow(FOLLOW_2);
            ruleEString();

            state._fsp--;

             after(grammarAccess.getEventPackAccess().getUnitEStringParserRuleCall_5_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__UnitAssignment_5"


    // $ANTLR start "rule__EventPack__EventsAssignment_6"
    // InternalRealmForgeDsl.g:2156:1: rule__EventPack__EventsAssignment_6 : ( ruleEvent ) ;
    public final void rule__EventPack__EventsAssignment_6() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2160:1: ( ( ruleEvent ) )
            // InternalRealmForgeDsl.g:2161:2: ( ruleEvent )
            {
            // InternalRealmForgeDsl.g:2161:2: ( ruleEvent )
            // InternalRealmForgeDsl.g:2162:3: ruleEvent
            {
             before(grammarAccess.getEventPackAccess().getEventsEventParserRuleCall_6_0()); 
            pushFollow(FOLLOW_2);
            ruleEvent();

            state._fsp--;

             after(grammarAccess.getEventPackAccess().getEventsEventParserRuleCall_6_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventPack__EventsAssignment_6"


    // $ANTLR start "rule__EventResult__MessageAssignment_2_1"
    // InternalRealmForgeDsl.g:2171:1: rule__EventResult__MessageAssignment_2_1 : ( ruleEString ) ;
    public final void rule__EventResult__MessageAssignment_2_1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2175:1: ( ( ruleEString ) )
            // InternalRealmForgeDsl.g:2176:2: ( ruleEString )
            {
            // InternalRealmForgeDsl.g:2176:2: ( ruleEString )
            // InternalRealmForgeDsl.g:2177:3: ruleEString
            {
             before(grammarAccess.getEventResultAccess().getMessageEStringParserRuleCall_2_1_0()); 
            pushFollow(FOLLOW_2);
            ruleEString();

            state._fsp--;

             after(grammarAccess.getEventResultAccess().getMessageEStringParserRuleCall_2_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__EventResult__MessageAssignment_2_1"


    // $ANTLR start "rule__TypeRacerEvent__IsCaseSensitiveAssignment_2"
    // InternalRealmForgeDsl.g:2186:1: rule__TypeRacerEvent__IsCaseSensitiveAssignment_2 : ( ( 'isCaseSensitive' ) ) ;
    public final void rule__TypeRacerEvent__IsCaseSensitiveAssignment_2() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2190:1: ( ( ( 'isCaseSensitive' ) ) )
            // InternalRealmForgeDsl.g:2191:2: ( ( 'isCaseSensitive' ) )
            {
            // InternalRealmForgeDsl.g:2191:2: ( ( 'isCaseSensitive' ) )
            // InternalRealmForgeDsl.g:2192:3: ( 'isCaseSensitive' )
            {
             before(grammarAccess.getTypeRacerEventAccess().getIsCaseSensitiveIsCaseSensitiveKeyword_2_0()); 
            // InternalRealmForgeDsl.g:2193:3: ( 'isCaseSensitive' )
            // InternalRealmForgeDsl.g:2194:4: 'isCaseSensitive'
            {
             before(grammarAccess.getTypeRacerEventAccess().getIsCaseSensitiveIsCaseSensitiveKeyword_2_0()); 
            match(input,35,FOLLOW_2); 
             after(grammarAccess.getTypeRacerEventAccess().getIsCaseSensitiveIsCaseSensitiveKeyword_2_0()); 

            }

             after(grammarAccess.getTypeRacerEventAccess().getIsCaseSensitiveIsCaseSensitiveKeyword_2_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__IsCaseSensitiveAssignment_2"


    // $ANTLR start "rule__TypeRacerEvent__DifficultyAssignment_4"
    // InternalRealmForgeDsl.g:2205:1: rule__TypeRacerEvent__DifficultyAssignment_4 : ( ruleDifficulty ) ;
    public final void rule__TypeRacerEvent__DifficultyAssignment_4() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2209:1: ( ( ruleDifficulty ) )
            // InternalRealmForgeDsl.g:2210:2: ( ruleDifficulty )
            {
            // InternalRealmForgeDsl.g:2210:2: ( ruleDifficulty )
            // InternalRealmForgeDsl.g:2211:3: ruleDifficulty
            {
             before(grammarAccess.getTypeRacerEventAccess().getDifficultyDifficultyEnumRuleCall_4_0()); 
            pushFollow(FOLLOW_2);
            ruleDifficulty();

            state._fsp--;

             after(grammarAccess.getTypeRacerEventAccess().getDifficultyDifficultyEnumRuleCall_4_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__DifficultyAssignment_4"


    // $ANTLR start "rule__TypeRacerEvent__TimeLimitAssignment_5_1"
    // InternalRealmForgeDsl.g:2220:1: rule__TypeRacerEvent__TimeLimitAssignment_5_1 : ( ruleEDoubleObject ) ;
    public final void rule__TypeRacerEvent__TimeLimitAssignment_5_1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2224:1: ( ( ruleEDoubleObject ) )
            // InternalRealmForgeDsl.g:2225:2: ( ruleEDoubleObject )
            {
            // InternalRealmForgeDsl.g:2225:2: ( ruleEDoubleObject )
            // InternalRealmForgeDsl.g:2226:3: ruleEDoubleObject
            {
             before(grammarAccess.getTypeRacerEventAccess().getTimeLimitEDoubleObjectParserRuleCall_5_1_0()); 
            pushFollow(FOLLOW_2);
            ruleEDoubleObject();

            state._fsp--;

             after(grammarAccess.getTypeRacerEventAccess().getTimeLimitEDoubleObjectParserRuleCall_5_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__TimeLimitAssignment_5_1"


    // $ANTLR start "rule__TypeRacerEvent__RetriesAssignment_7"
    // InternalRealmForgeDsl.g:2235:1: rule__TypeRacerEvent__RetriesAssignment_7 : ( ruleEInt ) ;
    public final void rule__TypeRacerEvent__RetriesAssignment_7() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2239:1: ( ( ruleEInt ) )
            // InternalRealmForgeDsl.g:2240:2: ( ruleEInt )
            {
            // InternalRealmForgeDsl.g:2240:2: ( ruleEInt )
            // InternalRealmForgeDsl.g:2241:3: ruleEInt
            {
             before(grammarAccess.getTypeRacerEventAccess().getRetriesEIntParserRuleCall_7_0()); 
            pushFollow(FOLLOW_2);
            ruleEInt();

            state._fsp--;

             after(grammarAccess.getTypeRacerEventAccess().getRetriesEIntParserRuleCall_7_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__RetriesAssignment_7"


    // $ANTLR start "rule__TypeRacerEvent__SentenceAssignment_9"
    // InternalRealmForgeDsl.g:2250:1: rule__TypeRacerEvent__SentenceAssignment_9 : ( ruleEString ) ;
    public final void rule__TypeRacerEvent__SentenceAssignment_9() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2254:1: ( ( ruleEString ) )
            // InternalRealmForgeDsl.g:2255:2: ( ruleEString )
            {
            // InternalRealmForgeDsl.g:2255:2: ( ruleEString )
            // InternalRealmForgeDsl.g:2256:3: ruleEString
            {
             before(grammarAccess.getTypeRacerEventAccess().getSentenceEStringParserRuleCall_9_0()); 
            pushFollow(FOLLOW_2);
            ruleEString();

            state._fsp--;

             after(grammarAccess.getTypeRacerEventAccess().getSentenceEStringParserRuleCall_9_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__SentenceAssignment_9"


    // $ANTLR start "rule__TypeRacerEvent__ResultAssignment_11"
    // InternalRealmForgeDsl.g:2265:1: rule__TypeRacerEvent__ResultAssignment_11 : ( ruleEventResult ) ;
    public final void rule__TypeRacerEvent__ResultAssignment_11() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2269:1: ( ( ruleEventResult ) )
            // InternalRealmForgeDsl.g:2270:2: ( ruleEventResult )
            {
            // InternalRealmForgeDsl.g:2270:2: ( ruleEventResult )
            // InternalRealmForgeDsl.g:2271:3: ruleEventResult
            {
             before(grammarAccess.getTypeRacerEventAccess().getResultEventResultParserRuleCall_11_0()); 
            pushFollow(FOLLOW_2);
            ruleEventResult();

            state._fsp--;

             after(grammarAccess.getTypeRacerEventAccess().getResultEventResultParserRuleCall_11_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__TypeRacerEvent__ResultAssignment_11"


    // $ANTLR start "rule__QuestionEvent__DifficultyAssignment_3"
    // InternalRealmForgeDsl.g:2280:1: rule__QuestionEvent__DifficultyAssignment_3 : ( ruleDifficulty ) ;
    public final void rule__QuestionEvent__DifficultyAssignment_3() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2284:1: ( ( ruleDifficulty ) )
            // InternalRealmForgeDsl.g:2285:2: ( ruleDifficulty )
            {
            // InternalRealmForgeDsl.g:2285:2: ( ruleDifficulty )
            // InternalRealmForgeDsl.g:2286:3: ruleDifficulty
            {
             before(grammarAccess.getQuestionEventAccess().getDifficultyDifficultyEnumRuleCall_3_0()); 
            pushFollow(FOLLOW_2);
            ruleDifficulty();

            state._fsp--;

             after(grammarAccess.getQuestionEventAccess().getDifficultyDifficultyEnumRuleCall_3_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__DifficultyAssignment_3"


    // $ANTLR start "rule__QuestionEvent__TimeLimitAssignment_4_1"
    // InternalRealmForgeDsl.g:2295:1: rule__QuestionEvent__TimeLimitAssignment_4_1 : ( ruleEDoubleObject ) ;
    public final void rule__QuestionEvent__TimeLimitAssignment_4_1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2299:1: ( ( ruleEDoubleObject ) )
            // InternalRealmForgeDsl.g:2300:2: ( ruleEDoubleObject )
            {
            // InternalRealmForgeDsl.g:2300:2: ( ruleEDoubleObject )
            // InternalRealmForgeDsl.g:2301:3: ruleEDoubleObject
            {
             before(grammarAccess.getQuestionEventAccess().getTimeLimitEDoubleObjectParserRuleCall_4_1_0()); 
            pushFollow(FOLLOW_2);
            ruleEDoubleObject();

            state._fsp--;

             after(grammarAccess.getQuestionEventAccess().getTimeLimitEDoubleObjectParserRuleCall_4_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__TimeLimitAssignment_4_1"


    // $ANTLR start "rule__QuestionEvent__RetriesAssignment_6"
    // InternalRealmForgeDsl.g:2310:1: rule__QuestionEvent__RetriesAssignment_6 : ( ruleEInt ) ;
    public final void rule__QuestionEvent__RetriesAssignment_6() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2314:1: ( ( ruleEInt ) )
            // InternalRealmForgeDsl.g:2315:2: ( ruleEInt )
            {
            // InternalRealmForgeDsl.g:2315:2: ( ruleEInt )
            // InternalRealmForgeDsl.g:2316:3: ruleEInt
            {
             before(grammarAccess.getQuestionEventAccess().getRetriesEIntParserRuleCall_6_0()); 
            pushFollow(FOLLOW_2);
            ruleEInt();

            state._fsp--;

             after(grammarAccess.getQuestionEventAccess().getRetriesEIntParserRuleCall_6_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__RetriesAssignment_6"


    // $ANTLR start "rule__QuestionEvent__QuestionAssignment_8"
    // InternalRealmForgeDsl.g:2325:1: rule__QuestionEvent__QuestionAssignment_8 : ( ruleEString ) ;
    public final void rule__QuestionEvent__QuestionAssignment_8() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2329:1: ( ( ruleEString ) )
            // InternalRealmForgeDsl.g:2330:2: ( ruleEString )
            {
            // InternalRealmForgeDsl.g:2330:2: ( ruleEString )
            // InternalRealmForgeDsl.g:2331:3: ruleEString
            {
             before(grammarAccess.getQuestionEventAccess().getQuestionEStringParserRuleCall_8_0()); 
            pushFollow(FOLLOW_2);
            ruleEString();

            state._fsp--;

             after(grammarAccess.getQuestionEventAccess().getQuestionEStringParserRuleCall_8_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__QuestionAssignment_8"


    // $ANTLR start "rule__QuestionEvent__ResultAssignment_10"
    // InternalRealmForgeDsl.g:2340:1: rule__QuestionEvent__ResultAssignment_10 : ( ruleEventResult ) ;
    public final void rule__QuestionEvent__ResultAssignment_10() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2344:1: ( ( ruleEventResult ) )
            // InternalRealmForgeDsl.g:2345:2: ( ruleEventResult )
            {
            // InternalRealmForgeDsl.g:2345:2: ( ruleEventResult )
            // InternalRealmForgeDsl.g:2346:3: ruleEventResult
            {
             before(grammarAccess.getQuestionEventAccess().getResultEventResultParserRuleCall_10_0()); 
            pushFollow(FOLLOW_2);
            ruleEventResult();

            state._fsp--;

             after(grammarAccess.getQuestionEventAccess().getResultEventResultParserRuleCall_10_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__ResultAssignment_10"


    // $ANTLR start "rule__QuestionEvent__OptionsAssignment_13"
    // InternalRealmForgeDsl.g:2355:1: rule__QuestionEvent__OptionsAssignment_13 : ( ruleOption ) ;
    public final void rule__QuestionEvent__OptionsAssignment_13() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2359:1: ( ( ruleOption ) )
            // InternalRealmForgeDsl.g:2360:2: ( ruleOption )
            {
            // InternalRealmForgeDsl.g:2360:2: ( ruleOption )
            // InternalRealmForgeDsl.g:2361:3: ruleOption
            {
             before(grammarAccess.getQuestionEventAccess().getOptionsOptionParserRuleCall_13_0()); 
            pushFollow(FOLLOW_2);
            ruleOption();

            state._fsp--;

             after(grammarAccess.getQuestionEventAccess().getOptionsOptionParserRuleCall_13_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__OptionsAssignment_13"


    // $ANTLR start "rule__QuestionEvent__OptionsAssignment_14_1"
    // InternalRealmForgeDsl.g:2370:1: rule__QuestionEvent__OptionsAssignment_14_1 : ( ruleOption ) ;
    public final void rule__QuestionEvent__OptionsAssignment_14_1() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2374:1: ( ( ruleOption ) )
            // InternalRealmForgeDsl.g:2375:2: ( ruleOption )
            {
            // InternalRealmForgeDsl.g:2375:2: ( ruleOption )
            // InternalRealmForgeDsl.g:2376:3: ruleOption
            {
             before(grammarAccess.getQuestionEventAccess().getOptionsOptionParserRuleCall_14_1_0()); 
            pushFollow(FOLLOW_2);
            ruleOption();

            state._fsp--;

             after(grammarAccess.getQuestionEventAccess().getOptionsOptionParserRuleCall_14_1_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__QuestionEvent__OptionsAssignment_14_1"


    // $ANTLR start "rule__Option__TextAssignment_2"
    // InternalRealmForgeDsl.g:2385:1: rule__Option__TextAssignment_2 : ( ruleEString ) ;
    public final void rule__Option__TextAssignment_2() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2389:1: ( ( ruleEString ) )
            // InternalRealmForgeDsl.g:2390:2: ( ruleEString )
            {
            // InternalRealmForgeDsl.g:2390:2: ( ruleEString )
            // InternalRealmForgeDsl.g:2391:3: ruleEString
            {
             before(grammarAccess.getOptionAccess().getTextEStringParserRuleCall_2_0()); 
            pushFollow(FOLLOW_2);
            ruleEString();

            state._fsp--;

             after(grammarAccess.getOptionAccess().getTextEStringParserRuleCall_2_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__TextAssignment_2"


    // $ANTLR start "rule__Option__IsCorrectAnswerAssignment_3"
    // InternalRealmForgeDsl.g:2400:1: rule__Option__IsCorrectAnswerAssignment_3 : ( ( 'isCorrectAnswer' ) ) ;
    public final void rule__Option__IsCorrectAnswerAssignment_3() throws RecognitionException {

        		int stackSize = keepStackSize();
        	
        try {
            // InternalRealmForgeDsl.g:2404:1: ( ( ( 'isCorrectAnswer' ) ) )
            // InternalRealmForgeDsl.g:2405:2: ( ( 'isCorrectAnswer' ) )
            {
            // InternalRealmForgeDsl.g:2405:2: ( ( 'isCorrectAnswer' ) )
            // InternalRealmForgeDsl.g:2406:3: ( 'isCorrectAnswer' )
            {
             before(grammarAccess.getOptionAccess().getIsCorrectAnswerIsCorrectAnswerKeyword_3_0()); 
            // InternalRealmForgeDsl.g:2407:3: ( 'isCorrectAnswer' )
            // InternalRealmForgeDsl.g:2408:4: 'isCorrectAnswer'
            {
             before(grammarAccess.getOptionAccess().getIsCorrectAnswerIsCorrectAnswerKeyword_3_0()); 
            match(input,36,FOLLOW_2); 
             after(grammarAccess.getOptionAccess().getIsCorrectAnswerIsCorrectAnswerKeyword_3_0()); 

            }

             after(grammarAccess.getOptionAccess().getIsCorrectAnswerIsCorrectAnswerKeyword_3_0()); 

            }


            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {

            	restoreStackSize(stackSize);

        }
        return ;
    }
    // $ANTLR end "rule__Option__IsCorrectAnswerAssignment_3"

    // Delegated rules


 

    public static final BitSet FOLLOW_1 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_2 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_3 = new BitSet(new long[]{0x0000000000000030L});
    public static final BitSet FOLLOW_4 = new BitSet(new long[]{0x0000000000020000L});
    public static final BitSet FOLLOW_5 = new BitSet(new long[]{0x0000000000040000L});
    public static final BitSet FOLLOW_6 = new BitSet(new long[]{0x0000000041000000L});
    public static final BitSet FOLLOW_7 = new BitSet(new long[]{0x0000000041000002L});
    public static final BitSet FOLLOW_8 = new BitSet(new long[]{0x0000000000100040L});
    public static final BitSet FOLLOW_9 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_10 = new BitSet(new long[]{0x0000000000001800L});
    public static final BitSet FOLLOW_11 = new BitSet(new long[]{0x0000000000080040L});
    public static final BitSet FOLLOW_12 = new BitSet(new long[]{0x0000000000200000L});
    public static final BitSet FOLLOW_13 = new BitSet(new long[]{0x0000000000C00000L});
    public static final BitSet FOLLOW_14 = new BitSet(new long[]{0x0000000802000000L});
    public static final BitSet FOLLOW_15 = new BitSet(new long[]{0x000000000000E000L});
    public static final BitSet FOLLOW_16 = new BitSet(new long[]{0x0000000024000000L});
    public static final BitSet FOLLOW_17 = new BitSet(new long[]{0x0000000008000000L});
    public static final BitSet FOLLOW_18 = new BitSet(new long[]{0x0000000010000000L});
    public static final BitSet FOLLOW_19 = new BitSet(new long[]{0x0000000000400000L});
    public static final BitSet FOLLOW_20 = new BitSet(new long[]{0x0000000000180040L});
    public static final BitSet FOLLOW_21 = new BitSet(new long[]{0x0000000002000000L});
    public static final BitSet FOLLOW_22 = new BitSet(new long[]{0x0000000080000000L});
    public static final BitSet FOLLOW_23 = new BitSet(new long[]{0x0000000100000000L});
    public static final BitSet FOLLOW_24 = new BitSet(new long[]{0x0000000200400000L});
    public static final BitSet FOLLOW_25 = new BitSet(new long[]{0x0000000200000002L});
    public static final BitSet FOLLOW_26 = new BitSet(new long[]{0x0000000400000000L});
    public static final BitSet FOLLOW_27 = new BitSet(new long[]{0x0000001000400000L});

}