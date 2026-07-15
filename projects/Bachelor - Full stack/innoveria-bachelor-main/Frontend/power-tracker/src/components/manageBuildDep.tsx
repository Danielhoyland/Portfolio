import { useEffect, useState } from 'react'
import { Button } from "@/components/ui/button";
import axios from 'axios';
import { redirect } from 'react-router-dom';
import "@/pages/adminSysConfig.css"
import { Separator } from "@/components/ui/separator"
import { Input } from "@/components/ui/input"
import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogCancel,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogFooter,
    AlertDialogHeader,
    AlertDialogTitle,
    AlertDialogTrigger,
} from "@/components/ui/alert-dialog"
import { Plus } from 'lucide-react';
import {
	Accordion,
	AccordionContent,
	AccordionItem,
	AccordionTrigger,
} from "@/components/ui/accordion"
import { Factory } from 'lucide-react';
import { Warehouse } from 'lucide-react';
import { Pencil } from 'lucide-react';
import { Label } from '@radix-ui/react-label';


const EgressAPI = import.meta.env.VITE_EAPI_URL
const IngressAPI = import.meta.env.VITE_IAPI_URL

function BuildDepSubpage () {
	//MANY USESTATES AND MORE
	const [buildDepData, setBuildDepData] = useState({
	buildingDepartmentData:
		[
			{
				building: {
					name: ""
				},
				departments: [
					{
						name: ""
					}
				]
			}
		]
	});

	const [buildingNameInput, setBuildingNameInput] = useState("");
	const [buildingBool, setBuildingBool] = useState(false);
	const [departmentNameInput, setDepartmentNameInput] = useState("");
	const [departmentBool, setDepartmentBool] = useState(false);
	const [buildingNameInputDP, setBuildingNameInputDP] = useState("");

	const [editIndex, setEditIndex] = useState(-1);
	const [editIndexDep, setEditIndexDep] = useState(-1);
	const [editIndexBuild, setEditIndexBuild] = useState(-1);
	const [text, setText] = useState('');
	const [oldtext, setOldText] = useState('');
	const [buildName, setBuildName] = useState('');

	const [textFocus, setTextFocus] = useState(false);
	const [buttonFocus, setButtonFocus] = useState(false);
	const [search, setSearch] = useState("");

	const handleEdit = (index: number, value: string) => {
		setEditIndex(index);
		setText(value);
	};

	const handleEditDep = (index1: number, index: number, value: string) => {
		setEditIndexDep(index1);
		setEditIndexBuild(index);
		setText(value);
	};

	const handleSaveBuilding = () => {
		setEditIndex(-1);
		editBuilding(oldtext, text)
	};

	const handleSaveDepartment = () => {
		setEditIndexDep(-1);
		editDepartment(oldtext, text, buildName)
	};

    const handleCancelEdit = () => {
        if (!buttonFocus && !textFocus) {
        setEditIndex(-1);
        }
    };

	useEffect(() => {
		fetchDataBuildingAndDepartment().then((buildingDepartments) => {
			setBuildDepData((prevBuildDepData) => {
				const updatedBuildingData = [...prevBuildDepData.buildingDepartmentData];

				buildingDepartments.forEach((buildingDepartment) => {
					// Check if the building name already exists in buildDepData
					const existingBuildingIndex = updatedBuildingData.findIndex((entry) => entry.building.name === buildingDepartment.buildingName);

					if (existingBuildingIndex !== -1) {
						// Building name already exists
						const existingBuilding = updatedBuildingData[existingBuildingIndex];
						const existingDepartment = existingBuilding.departments.find(department => department.name === buildingDepartment.departments);

						if (!existingDepartment) {
							// Department doesn't exist for this building, add it
							existingBuilding.departments.push({
								name: buildingDepartment.departments
							});
						}
					} else {
						// Building name doesn't exist, create a new entry
						updatedBuildingData.push({
							building: {
								name: buildingDepartment.buildingName
							},
							departments: Array.isArray(buildingDepartment.departments) ? buildingDepartment.departments.map(department => ({ name: department.name })) : [{ name: buildingDepartment.departments }]
						});
					}
				});
				return {
					...prevBuildDepData,
					buildingDepartmentData: updatedBuildingData
				};
			});
		});
	}, []);
	const [open, setOpen] = useState(false);

	const handleClose = () => {
		setOpen(false);
	};

	
//ADD NEW DATA
const addBuilding = (e: React.FormEvent<HTMLFormElement>, buildingName: string) =>  {
	e.preventDefault()

	var token: string = ""
	var tokenBool = sessionStorage.getItem("TOKEN")
	if (tokenBool == null) {
		redirect('/')
	} else {
		token = tokenBool
	}
	console.log('URL To call: ' + IngressAPI + 'token used' + token)
	axios.post(
		IngressAPI + '/new-building',
		{
			building_name: buildingName,
			sessionToken: token,
		})
	.then((res)=>{
		console.log(res.data)
	}).catch((error) => {
		console.log(error)
		setOpen(true)
	})
}

const addDepartment = ( buildingName: string, departmentName: string) =>  {

	var token: string = ""
	var tokenBool = sessionStorage.getItem("TOKEN")
	if (tokenBool == null) {
		redirect('/')
	} else {
		token = tokenBool
	}
	console.log('URL To call: ' + IngressAPI + 'token used' + token)
	axios.post(
		IngressAPI + '/new-department',
		{
			building_name: buildingName,
			department_name: departmentName,
			sessionToken: token,
		})
	.then((res)=>{
		console.log(res.data)
	}).catch((error) => {
		console.log(error)
		setOpen(true)
	})
}

  ///EDIT THE DATA
const editBuilding= ( oldBuildingName: string, newBuildingName: string) =>  {


	var token: string = ""
	var tokenBool = sessionStorage.getItem("TOKEN")
	if (tokenBool == null) {
		redirect('/')
	} else {
		token = tokenBool
	}
	console.log('URL To call: ' + IngressAPI + 'token used' + token)
	axios.put(
		IngressAPI + '/new-building',
		{
			old_building_name: oldBuildingName,
			new_building_name: newBuildingName,
			sessionToken: token,
		})
	.then((res)=>{
		console.log(res.data)
	}).catch((error) => {
		console.log(error)
		setOpen(true)
	})
}
const editDepartment= ( oldDepartmentName: string, newDepartmentName: string, buildingName: string) =>  {


	var token: string = ""
	var tokenBool = sessionStorage.getItem("TOKEN")
	if (tokenBool == null) {
		redirect('/')
	} else {
		token = tokenBool
	}
	console.log('URL To call: ' + IngressAPI + 'token used' + token)
	axios.put(
		IngressAPI + '/new-department',
		{
			building_name: buildingName,
			old_department_name: oldDepartmentName,
			new_department_name: newDepartmentName,
			sessionToken: token,
		})
	.then((res)=>{
		console.log(res.data)
	}).catch((error) => {
		console.log(error)
		setOpen(true)
	})
}

const deleteBuilding = ( name: string) =>  {


	var token: string = ""
	var tokenBool = sessionStorage.getItem("TOKEN")
	if (tokenBool == null) {
		redirect('/')
	} else {
		token = tokenBool
	}
	console.log('URL To call: ' + IngressAPI + 'token used' + token)
	axios.delete(
		IngressAPI + '/new-building',
		{
		data:{
			building_name: name,
			sessionToken: token,
			}
		})
	.then((res)=>{
		console.log(res.data)
	}).catch((error) => {
		console.log(error)
		setOpen(true)
	})
}
const deleteDepartment= ( name: string) =>  {


	var token: string = ""
	var tokenBool = sessionStorage.getItem("TOKEN")
	if (tokenBool == null) {
		redirect('/')
	} else {
		token = tokenBool
	}
	console.log('URL To call: ' + IngressAPI + 'token used' + token)
	axios.delete(
		IngressAPI + '/new-department',
		{
		data:{
			dep_name: name,
			sessionToken: token,
			}
		})
	.then((res)=>{
		console.log(res.data)
	}).catch((error) => {
		console.log(error)
		setOpen(true)
	})
}

  // GET THE DATA
class BuildingDep {
	buildingName: string = "";
	departments: string = "";

	constructor(buildingName: string, departments: string) {
		this.buildingName = buildingName;
		this.departments = departments;
	}
}

async function fetchDataBuildingAndDepartment(): Promise<BuildingDep[]> {
	var buiDep: BuildingDep[] = []
	await axios.post(EgressAPI + '/buildDep',
	{
	sessionToken: sessionStorage.getItem('TOKEN')
	}).then((res) => {
		const buildingDepData = res.data.buildingDep;

		for (const buildingName in buildingDepData) {
			const departments = buildingDepData[buildingName];
			if (Array.isArray(departments)) {
				departments.forEach((department: any) => {
					buiDep.push(new BuildingDep(buildingName, department.name));
				});
			}
		}
	}).catch((error) => {
	console.log(error)
	setOpen(true)
	})

	return buiDep
}

type BuildDepData = {
	buildingDepartmentData: {
		building: {
			name: string;
		};
		departments: {
			name: string;
		}[];
	}[];
};

type SetBuildDepData = React.Dispatch<React.SetStateAction<{
	buildingDepartmentData: {
		building: {
			name: string;
		};
		departments: {
			name: string;
		}[];
	}[];
}>>;

const usePopoutState = () => {
	const [isPopoutOpen, setPopoutOpen] = useState(false);
	const [whatOperation, setOperation] = useState(0);
	const [assignValue, setassignValue] = useState(0);



	const handleOpenPopout = () => {
	setPopoutOpen(true);
	};

	const handleClosePopout = () => {
	setPopoutOpen(false);
	};

	const handleChangeOperation = (op:number) => {
	setOperation(op);
	};

	return {
	isPopoutOpen,
	handleOpenPopout,
	handleClosePopout,
	whatOperation,
	handleChangeOperation,
	assignValue,
	setassignValue,
	};
};


interface ManagePopup {
	handleChangeOperation: (op: number) => void;
	setassignValue: React.Dispatch<React.SetStateAction<number>>;
}
interface DisplayBuildingDepartmentDataProps extends ManagePopup {
	data: BuildDepData;
	setData: SetBuildDepData;
}


	return (
		<div className='flex h-[100%]'>
            <AlertDialog open={open}>
            <AlertDialogContent>
            <AlertDialogHeader>
                <AlertDialogTitle>Something went wrong</AlertDialogTitle>
            </AlertDialogHeader>
                <div>
                    Please try again or check your connection
                </div>
            <AlertDialogFooter>
                <AlertDialogCancel onClick={handleClose}>Close</AlertDialogCancel>
            </AlertDialogFooter>
            </AlertDialogContent>
        </AlertDialog>
			<div className='w-[1100px]'>
				{/* Title and search bar */}
				<div className="w-[100%] h-[40px] flex justify-between content-center" style={{ marginTop: '5px' }}>
					<h1 className="scroll-m-20 text-2xl font-semibold tracking-tight">Buildings and Departments</h1>
					<Input 
						className="h-[30px] w-[200px]" 
						type="text" 
						value={search} 
						onChange={(event) => setSearch(event.target.value) } 
						placeholder="Search.."
					/>
				</div>

				<Separator className="mb-[10px]" />	

				{/* New building button */}
				<AlertDialog>
					<AlertDialogTrigger> 
						<Button className="w-[60px] h-[30px] mb-[10px] ml-[10px]" variant="default" size="icon" onClick={() => { setBuildingBool(!buildingBool)}} title="Add Building">
							<Factory className="h-[20px]" />
							<Plus className="h-[18px]" />
						</Button>
					</AlertDialogTrigger>
					<AlertDialogContent>
						<form onSubmit={(e) => { if(buildingNameInput!=""){addBuilding(e,buildingNameInput); setBuildingNameInput(""); setBuildingBool(!buildingBool);}}} className="form-container">
						<Input className="mb-[20px]" type="text" placeholder="Buidling Name" value={buildingNameInput} onChange={(event) => setBuildingNameInput(event.target.value)} required/>
						<AlertDialogFooter>
							<AlertDialogAction>
								<Button 
									type="submit" 
									onClick={() => {
									setBuildDepData(prevState => ({
									...prevState,
									buildingDepartmentData: [
										...prevState.buildingDepartmentData,
										{
											building: {
												name: buildingNameInput
											},
											departments: []
										}
									]
									}));
								}}>Submit
								</Button>
							</AlertDialogAction>
							<AlertDialogCancel>Cancel</AlertDialogCancel>
						</AlertDialogFooter>
						</form>
					</AlertDialogContent>
				</AlertDialog>	

				{/* List of buildings */}	
				<Accordion type="single" collapsible>
				{Object.entries(buildDepData.buildingDepartmentData).map(([id, buildDep], index) => (
					
					<AccordionItem value={id}>
					<div key={id}>
						{buildDep.building.name !== "" && (buildDep.building.name.toLowerCase().includes(search.toLowerCase())||buildDep.departments.some(department => department.name.toLowerCase().includes(search.toLowerCase()))) && (
							<div>
								{/* Building Name */}
								<div className="buildingbar">
									<Separator orientation='vertical' className="mr-[5px]"/>
									<AccordionTrigger>
										<div className='flex flex-row'>
											<Factory className="mx-[20px]" />
											<h4 className="scroll-m-20 text-xl font-semibold tracking-tight">{buildDep.building.name}</h4>
										</div> 
									</AccordionTrigger>
									<Separator orientation='vertical' className="mx-[5px]"/>
									<div className="h-[100%] flex items-center justify-around">
										<AlertDialog>
											<AlertDialogTrigger asChild>
												<Button className="h-[30px] w-[60px]" variant="outline" onClick={()=>{setBuildingNameInput(buildDep.building.name)}}>Edit</Button>
											</AlertDialogTrigger>
											<AlertDialogContent> 
												<AlertDialogHeader>Edit {buildDep.building.name}</AlertDialogHeader>
												<form onSubmit={(e)=>{e.preventDefault();editBuilding(buildDep.building.name, buildingNameInput); setBuildingNameInput("")}}>
													<Input className="mb-[20px]" type="text" placeholder="Buidling Name" value={buildingNameInput} onChange={(event) => setBuildingNameInput(event.target.value)} required/>
													<AlertDialogAction type='submit'>Confirm</AlertDialogAction>
													<AlertDialogCancel onClick={()=>{setBuildingNameInput("")}}>Cancel</AlertDialogCancel>
												</form>
											</AlertDialogContent>
										</AlertDialog>
										<AlertDialog>
											<AlertDialogTrigger asChild>
												<Button className="h-[30px] w-[60px]" variant="outline" >Delete</Button>
											</AlertDialogTrigger>
											<AlertDialogContent> 
												<AlertDialogHeader>Are you sure about deleting {buildDep.building.name} and all its machines and departments?</AlertDialogHeader>
												<AlertDialogAction onClick={()=>deleteBuilding(buildDep.building.name)}>DELETE</AlertDialogAction>
												<AlertDialogCancel>Cancel</AlertDialogCancel>
											</AlertDialogContent>
										</AlertDialog>
										
									</div>
									<Separator orientation='vertical' className="ml-[5px]"/>
								</div>
								{/* In building dropdown */}
								<AccordionContent>
								{/* Add department button */}	
								<div className="depbar">
									<Separator orientation='vertical' className="mr-[5px]"/>
									<AlertDialog>
										<AlertDialogTrigger asChild>
											<Button className="w-[60px] h-[30px] mb-[10px] ml-[20px] mt-[10px]" variant="default" size="icon" onClick={() => {
														setDepartmentBool(!departmentBool);
														setBuildingNameInputDP(buildDep.building.name)
													}
												}
												title="Add Department to this building">
												<Warehouse className="h-[20px]" />
												<Plus className="h-[18px]" />
											</Button>
										</AlertDialogTrigger>
										<AlertDialogContent> 
											<form onSubmit={(e) => {
												e.preventDefault();
												console.log(departmentNameInput)
												if (departmentNameInput !== "") {
													addDepartment(buildDep.building.name, departmentNameInput);
													setDepartmentNameInput("");
													setBuildingNameInputDP("");
													setDepartmentBool(!departmentBool);
												}
											}}>
												<AlertDialogHeader>
													<h4 className="scroll-m-20 text-xl font-semibold tracking-tight">
														Add Department to: {buildDep.building.name}
													</h4>
												</AlertDialogHeader>
												<div className="flex items-center space-x-2">
													<Label htmlFor='addDepName'>Name</Label>
													<Input id="addDepName" className="my-[20px] w-[200px]" required value={departmentNameInput} onChange={(event) => setDepartmentNameInput(event.target.value)}/>
												</div>
												<AlertDialogFooter>
													<AlertDialogAction>
														<Button 
															type="submit" 
															onClick={() => {
															setBuildDepData(prevState => ({
															...prevState,
															buildingDepartmentData: [
																...prevState.buildingDepartmentData,
																{
																	building: {
																		name: buildingNameInput
																	},
																	departments: []
																}
															]
															}));
														}}>Submit
														</Button>
													</AlertDialogAction>
													<AlertDialogCancel>Cancel</AlertDialogCancel>
												</AlertDialogFooter>
											</form>
										</AlertDialogContent>
									</AlertDialog>
									<div/> <div/> <div/>
									<Separator orientation='vertical' className="mx-[5px]"/>
									<div/>
									<Separator orientation='vertical' className="ml-[5px]"/>
								</div>
								{/* Department list */}	 
								<ul>
									{buildDep.departments.map((department, index1) => (
										<li key={index1}>		
											<div className="depbar">
												<Separator orientation='vertical' className="mr-[5px]"/>
												<p className="h-[35px] flex leading-7 items-start items-center">
													<Warehouse className="h-[20px] mr-[20px] ml-[40px]" />
													<h5 className="scroll-m-20 text-lg tracking-tight">{department.name}</h5>
												</p>
												<div className='flex justify-center'>
													<p className="text-sm text-muted-foreground">...</p>
												</div>
												<div/>
												<div className="h-[100%] flex items-center justify-around">
													<AlertDialog>
														<AlertDialogTrigger asChild>
															<Button className="h-[30px] w-[60px]" variant="outline" onClick={()=>{setDepartmentNameInput(department.name)}}>Edit</Button>
														</AlertDialogTrigger>
														<AlertDialogContent> 
															<AlertDialogHeader>Edit {department.name}</AlertDialogHeader>
															<form onSubmit={(e)=>{e.preventDefault();editDepartment(department.name, departmentNameInput,buildDep.building.name);setDepartmentNameInput("")}}>
																<Input className="mb-[20px]" type="text" placeholder="Buidling Name" value={departmentNameInput} onChange={(event) => setDepartmentNameInput(event.target.value)} required/>
																<AlertDialogAction type='submit'>Confirm</AlertDialogAction>
																<AlertDialogCancel onClick={()=>{setDepartmentNameInput("")}}>Cancel</AlertDialogCancel>
															</form>
														</AlertDialogContent>
													</AlertDialog>
													<AlertDialog>
														<AlertDialogTrigger asChild>
															<Button className="h-[30px] w-[60px]" variant="outline" >Delete</Button>
														</AlertDialogTrigger>
														<AlertDialogContent> 
															<AlertDialogHeader>Are you sure about deleting {department.name}? This will remove it from alle machines that has this department.</AlertDialogHeader>
															<AlertDialogAction onClick={()=>deleteDepartment(department.name)}>DELETE</AlertDialogAction>
															<AlertDialogCancel>Cancel</AlertDialogCancel>
														</AlertDialogContent>
													</AlertDialog>
												</div>
												<Separator orientation='vertical' className="mx-[5px]"/>
												<div></div>
												<Separator orientation='vertical' className="ml-[5px]"/>
											</div>
										</li>
									))}
								</ul>
								{/*{departmentBool && buildDep.building.name === buildingNameInputDP && (
									<form onSubmit={(e) => {
										if (departmentNameInput !== "") {
											addDepartment(e, buildDep.building.name, departmentNameInput);
											setDepartmentNameInput("");
											setBuildingNameInputDP("");
											setDepartmentBool(!departmentBool);
										}}} className="form-container">
										<input
											type="text"
											placeholder="Department Name"
											value={departmentNameInput}
											onChange={(event) => setDepartmentNameInput(event.target.value)}
											required
										/>
										<button type="submit" onClick={() => {
											const updatedBuildingData = [...buildDepData.buildingDepartmentData];
											updatedBuildingData[index].departments.push({ name: departmentNameInput });
											// Update the state with the modified building data
											setBuildDepData({
												...buildDep,
												buildingDepartmentData: updatedBuildingData
											});
											}}>Submit
										</button>
									</form>
								)}*/}
								<div className="h-[10px] grid grid-cols-[auto_1fr_auto_130px_auto]">
									<Separator orientation='vertical' className="mr-[5px]"/>
									<div/>
									<Separator orientation='vertical' className="mx-[5px]"/>
									<div/>
									<Separator orientation='vertical' className="ml-[5px]"/>
								</div>
								<Separator/>
								</AccordionContent>
							</div>
						)}
					</div>
					</AccordionItem>
				))}
				</Accordion>
			</div>

			<Separator className='mx-10' orientation="vertical"/> {/* Needs parent container to be set to 100% height for it to work as it takes on the height of parent container */}

			<div className="w-[400px]">

			</div>
            
		</div>
	);
};



export default BuildDepSubpage