package generator

import (
	"convert/lexer"
	"convert/parser"
	"testing"
)

func TestGenerator(t *testing.T) {
	input := `
array xrn {256} xrn1-xrn256;
array livwthn {16} livwth1-livwth16;
array marstan {16} marsta1-marsta16;
array marchkn {16} marchk1-marchk16;
array hohn {16} hoh1-hoh16;
array sexn {16} sex1-sex16;
array cry01n {16} cry1-cry16;
array cryo7n {16} cryoo1-cryoo16;
array ehatn {16} ehat1-ehat16;
array agen {16} age1-age16;
array hrpidn {16} hrpid1-hrpid16;
array persn {16} persno1-persno16;
array inecacn {16} inecac1-inecac16;
array caindn {16} caind1-caind16;
array ftptn {16} ftpt1-ftpt16;
array curedn {16} curedn1-curedn16;
array hohidn {16} hohid1-hohid16;
array famunitn {16} famunit1-famunit16;
array smsxfun {16} smsxfu1-smsxfu16;
array extend {16} ext1-ext16;
array fdpch19n {16} fdpch19n1-fdpch19n16;
array relhfun {16} relhfu1-relhfu16;
array extfun {16} extfu1-extfu16;
array futypen {16} futypen1-futypen16;
array recnon {16} recno1-recno16;
array xrel2 {16,16} xrn1-xrn256;

length ADDLEVEL $2;

do i = 1 to 16;
    smsxfun{i} = famunitn{i};
    smsxfun{i} = famunitn{i};
end;

do i = 1 to 16;
    do j = i to 16;

        if livwthn{i} = 3 and livwthn{j} = 3 then do;
            if (j > i) and (xrel2{j,i} in (2,20)) then do;
                if famunitn{j} > famunitn{i} then do;
                    Oldfamunit = famunitn{j};
                    Newfamunit = famunitn{i};
                
                    do k = 1 to 16;
                        if famunitn{k} = Oldfamunit then smsxfun{k} = Newfamunit;
                        else if famunitn{k} > Newfamunit then smsxfun{k} = smsxfun{k} - 1;
                        end;
                    end;
                end;
            else if (j < i) and (xrel2{i,j} in (2,20)) then do;
                if famunitn{j} > famunitn{i} then do; 
                    Oldfamunit = famunitn{j};
                    Newfamunit = famunitn{i};
                
                    do k = 1 to 16;
                        if famunitn{k} = Oldfamunit then smsxfun{k}=Newfamunit;
                            else if famunitn{k} > Newfamunit then smsxfun{k} = smsxfun{k}-1;
                        end;
                    end; 
                end;
            end;

        if marstan{i}=6 and marstan{j}=6 then do;
            if (j > i) and (xrel2{j,i} in (2,20)) then do;
                if famunitn{j}>famunitn{i} then do; 
                    Oldfamunit=famunitn{j};
                    Newfamunit=famunitn{i};
                
                    do k = 1 to 16;
                        if famunitn{k}=Oldfamunit then smsxfun{k}=Newfamunit;
                        else if famunitn{k}>Newfamunit then smsxfun{k}=smsxfun{k}-1;
                        end;
                    end;
                end;
            else if (j < i) and (xrel2{i,j} in (2,20)) then do;
                if famunitn{j}>famunitn{i} then do; 
                    Oldfamunit=famunitn{j};
                    Newfamunit=famunitn{i};
                
                    do k = 1 to 16;
                        if famunitn{k}=Oldfamunit then smsxfun{k}=Newfamunit;
                        else if famunitn{k}>Newfamunit then smsxfun{k}=smsxfun{k}-1;
                    end;
                end; 
            end;
        end;  
    end;
end;

SMSXFU = smsxfun{recno};
`
	l := lexer.New(input)
	p := parser.New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}
	g := NewGenerator(program, "dv_name")

	//print(g.GeneratePreamble().PythonRepresentation())

	s := g.Generate().TargetCodeRepresentation()
	print(s)
}

func checkParserErrors(t *testing.T, p *parser.Parser) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}
