"use client"

import * as React from "react"
import { format } from "date-fns"
import { Calendar as CalendarIcon } from "lucide-react"

import { cn } from "@/lib/utils"
import { Button } from "@/components/ui/button"
import { Calendar } from "@/components/ui/calendar"
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover"

type Props = {
    dates: Date | undefined
    setDate: React.Dispatch<React.SetStateAction<Date | undefined>>
    allowedDate: Date | undefined
    width?: Number
}

export function DatePicker({dates, setDate, allowedDate, width = 280} : Props) {
  var aDate: Date
  if (allowedDate == undefined) {
    aDate = new Date()
  }else {
    aDate = allowedDate
  }
  const widthS = width.toString() + "px"
  return (
    <Popover>
      <PopoverTrigger asChild>
      <Button
        variant={"outline"}
        style={{ width: widthS }} // custom code to chnage datepicker width
        className={cn(
          `justify-start text-left font-normal`, // Use backticks here
          !dates && `text-muted-foreground`
        )}
      >
          <CalendarIcon className="mr-2 h-4 w-4" />
          {dates ? format(dates, "PPP") : <span>Pick a date</span>}
        </Button>
      </PopoverTrigger>
      <PopoverContent className="w-auto p-0">
        <Calendar
          mode="single"
          selected={dates}
          onSelect={setDate}
          disabled={(date) =>
            date > aDate || date < new Date("1900-01-01")
          }
          initialFocus
        />
      </PopoverContent>
    </Popover>
  )
}
