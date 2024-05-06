
import TopBar from '@/components/topbar';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Button } from '@/components/ui/button';
import { Separator } from '@/components/ui/separator';

const MainComponent = () =>{

    return (
        <>
            <TopBar></TopBar>
            <main>

                <div className='leftbar'>

                    <h2 className="scroll-m-20 border-b pb-2 text-3xl font-semibold tracking-tight first:mt-0">Index</h2>
                    <br/>
                    
                    {/* LIST OF HEADERS */}
                    <ScrollArea className='h-[90%]'>
                        <div> {/* HEADER */}
                            <Button className='py-[20px] my-[5px]' variant="ghost" >
                                <h3 className="scroll-m-20 text-2xl font-semibold tracking-tight">
                                    Initial Setup
                                </h3>
                            </Button>
                        </div>
                        <div> {/* SUBHEADER */}
                            <Button className="ml-[30px] py-[20px] my-[5px]" variant="ghost">
                                <h4 className="scroll-m-20 text-xl font-semibold tracking-tight">
                                    Setting up Gateway
                                </h4>
                            </Button>
                        </div>
                        <div> {/* SUBHEADER */}
                            <Button className="ml-[30px] py-[20px] my-[5px]" variant="ghost" >
                                <h4 className="scroll-m-20 text-xl font-semibold tracking-tight">
                                    Adding Sensors
                                </h4>
                            </Button>
                        </div>

                        <br/> <Separator /> <br/>

                        <div> {/* HEADER */}
                            <Button className='py-[20px] my-[5px]' variant="ghost" >
                                <h3 className="scroll-m-20 text-2xl font-semibold tracking-tight">
                                    Sorting sensors
                                </h3>
                            </Button>
                        </div>
                        <div> {/* SUBHEADER */}
                            <Button className="ml-[30px] py-[20px] my-[5px]" variant="ghost" >
                                <h4 className="scroll-m-20 text-xl font-semibold tracking-tight">
                                    Building / Departments
                                </h4>
                            </Button>
                        </div>
                        <div> {/* SUBHEADER */}
                            <Button className="ml-[30px] py-[20px] my-[5px]" variant="ghost" >
                                <h4 className="scroll-m-20 text-xl font-semibold tracking-tight">
                                    Production Lines
                                </h4>
                            </Button>
                        </div>

                    </ScrollArea>

                </div>
                <div className='rightbar'>
                    <div className="flex justify-between">
                        <div className="h-[100%] w-[50px]">
                            <Button variant="secondary">^</Button>
                        </div>

                        <Separator orientation="vertical" />

                        {/* INFORMATION */}
                        <ScrollArea className="h-[100%] w-[50%] ml-[50px]">

                            <h2 className="scroll-m-20 border-b pb-2 text-3xl font-semibold tracking-tight first:mt-0">
                                Initial Setup
                            </h2> <br/>
                            <div className="">
                                <h3 className="scroll-m-20 text-2xl font-semibold tracking-tight">
                                    Setting up a Gateway
                                </h3> <br/>
                                <p>
                                    Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                                    Donec ut arcu accumsan, vulputate metus nec, tincidunt arcu. 
                                    Nulla nec varius odio, ac convallis tellus. Suspendisse potenti. 
                                    Morbi laoreet mollis ante, sed finibus ipsum auctor at. 
                                    Morbi dignissim quis felis nec aliquam. 
                                    Quisque at nunc in dui pulvinar bibendum tincidunt eu justo. 
                                    Interdum et malesuada fames ac ante ipsum primis in faucibus. 
                                </p>
                            </div>

                        </ScrollArea>
                    </div>
                </div>
            </main>
        </>    
    );
};
  
  export default MainComponent