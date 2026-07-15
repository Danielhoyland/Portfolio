interface testData {
    sensor: sensorData[],
    gateway: gatewayData[]
}

interface sensorData {
    eui: string,
    machine_name: string,
    expected_use: number,
    machineNr: string,
    building_id: number,
    building_name: string,
    department_id: number,
    department_name: string
}

interface gatewayData {
    eui_gate: string,
    comp_id: number
}